use crate::{
    instruction::Instruction,
    value::{bytes_put_value, ForeignFunc, Func, Value},
    vm::VM,
    IS_64BIT, WORD_SIZE,
};
use bytes::{BufMut, BytesMut};
use chili_ast::ty::{align::AlignOf, size::SizeOf, *};
use libffi::{
    low::{
        closure_alloc, closure_free, ffi_abi_FFI_DEFAULT_ABI as ABI, ffi_cif, ffi_type, prep_cif,
        prep_cif_var, prep_closure, type_tag, types, CodePtr,
    },
    raw::ffi_closure,
};
use libloading::Symbol;
use std::ffi::c_void;
use ustr::{ustr, Ustr, UstrMap};

macro_rules! raw_ptr {
    ($value: expr) => {
        $value as *mut _ as RawPointer
    };
}

macro_rules! ffi_type {
    ($value: expr) => {
        &mut $value as TypePointer
    };
}

pub type RawPointer = *mut c_void;
pub type TypePointer = *mut ffi_type;

pub(crate) struct Ffi {
    libs: UstrMap<libloading::Library>,
}

impl Ffi {
    pub(crate) fn new() -> Self {
        Self {
            libs: UstrMap::default(),
        }
    }

    pub(crate) unsafe fn load_lib(&mut self, lib_path: Ustr) -> &libloading::Library {
        // TODO: default libc should depend on the current platform
        let lib_name = match lib_path.as_str() {
            "c" | "C" | "libucrt" => ustr("msvcrt"),
            _ => lib_path,
        };

        self.libs
            .entry(lib_name)
            .or_insert_with(|| libloading::Library::new(lib_name.as_str()).unwrap())
    }

    pub(crate) unsafe fn load_symbol(&mut self, lib_path: Ustr, name: Ustr) -> Symbol<*mut c_void> {
        let lib = self.load_lib(lib_path);
        lib.get(name.as_bytes()).unwrap()
    }

    pub(crate) unsafe fn call<'vm>(
        &mut self,
        vm: *mut VM<'vm>,
        func: ForeignFunc,
        mut args: Vec<Value>,
    ) -> Value {
        let symbol = self.load_symbol(func.lib_path, func.name);

        let mut function = if func.variadic {
            let variadic_arg_types: Vec<TyKind> = args
                .iter()
                .skip(func.param_tys.len())
                .map(Value::get_ty_kind)
                .collect();

            Function::new_variadic(&func.param_tys, &variadic_arg_types, &func.return_ty)
        } else {
            Function::new(&func.param_tys, &func.return_ty)
        };

        let result = function.call(*symbol, &mut args, self, vm);

        // TODO: free used closures
        // TODO: clear closures vec

        Value::from_type_and_ptr(&func.return_ty, result as RawPointer)
    }
}

#[derive(Debug)]
struct Function {
    cif: ffi_cif,
    arg_types: Vec<TypePointer>,
}

impl Function {
    unsafe fn new(arg_types: &[TyKind], return_type: &TyKind) -> Self {
        let mut cif = ffi_cif::default();

        let return_type = return_type.as_ffi_type();

        let mut arg_types: Vec<TypePointer> =
            arg_types.iter().map(|arg| arg.as_ffi_type()).collect();

        prep_cif(
            &mut cif,
            ABI,
            arg_types.len(),
            return_type,
            arg_types.as_mut_ptr(),
        )
        .unwrap();

        Self { cif, arg_types }
    }

    unsafe fn new_variadic(
        arg_types: &[TyKind],
        variadic_arg_types: &[TyKind],
        return_type: &TyKind,
    ) -> Self {
        let mut cif = ffi_cif::default();

        let return_type = return_type.as_ffi_type();

        let mut cif_arg_types: Vec<TypePointer> = arg_types
            .iter()
            .chain(variadic_arg_types.iter())
            .map(|arg| arg.as_ffi_type())
            .collect();

        prep_cif_var(
            &mut cif,
            ABI,
            arg_types.len(),
            cif_arg_types.len(),
            return_type,
            cif_arg_types.as_mut_ptr(),
        )
        .unwrap();

        Self {
            cif,
            arg_types: cif_arg_types,
        }
    }

    unsafe fn call<'vm>(
        &mut self,
        fun: *const c_void,
        arg_values: &mut [Value],
        ffi: &mut Ffi,
        vm: *mut VM<'vm>,
    ) -> RawPointer {
        let code_ptr = CodePtr::from_ptr(fun);

        let mut args: Vec<RawPointer> = Vec::with_capacity(arg_values.len());
        let mut closures: Vec<*mut ffi_closure> = vec![];

        for (arg, arg_type) in arg_values.iter_mut().zip(self.arg_types.iter()) {
            let arg_type = **arg_type;
            let size = arg_type.size;
            let alignment = arg_type.alignment as usize;

            let arg_ptr = match arg {
                Value::I8(v) => raw_ptr!(v),
                Value::I16(v) => raw_ptr!(v),
                Value::I32(v) => raw_ptr!(v),
                Value::I64(v) => raw_ptr!(v),
                Value::Int(v) => raw_ptr!(v),
                Value::U8(v) => raw_ptr!(v),
                Value::U16(v) => raw_ptr!(v),
                Value::U32(v) => raw_ptr!(v),
                Value::U64(v) => raw_ptr!(v),
                Value::Uint(v) => raw_ptr!(v),
                Value::Bool(v) => raw_ptr!(v),
                Value::F32(v) => raw_ptr!(v),
                Value::F64(v) => raw_ptr!(v),
                Value::Aggregate(v) => {
                    let bytes = BytesMut::with_capacity(size);
                    let mut bytes = bytes.clone();

                    for value in v.elements.iter() {
                        bytes_put_value(&mut bytes, value);

                        // TODO: this could be more efficient
                        let value_size = (*value.get_ty_kind().as_ffi_type()).size;
                        if value_size < alignment {
                            let padding = alignment - value_size;
                            bytes.put_bytes(0, padding);
                        }
                    }

                    // Note (Ron): this clone could be useless, need to test this
                    bytes.as_mut_ptr() as RawPointer
                }
                Value::Array(v) => v.bytes.as_mut_ptr() as RawPointer,
                Value::Pointer(ptr) => raw_ptr!(ptr.as_raw()),
                Value::Func(func) => {
                    let (closure, code_ptr) = closure_alloc();

                    closures.push(closure);

                    let mut function = Box::new(Function::new(&func.arg_types, &func.return_type));

                    let user_data = ClosureUserData { vm, func };

                    prep_closure(
                        closure,
                        &mut function.cif as _,
                        closure_callback,
                        &user_data,
                        code_ptr,
                    )
                    .unwrap();

                    raw_ptr!(&mut code_ptr.as_mut_ptr())
                }
                Value::ForeignFunc(func) => {
                    let symbol = ffi.load_symbol(func.lib_path, func.name);
                    let mut ptr = *symbol;
                    raw_ptr!(&mut ptr)
                }
                _ => panic!("can't pass `{}` through ffi", arg.to_string()),
            };

            args.push(arg_ptr);
        }

        let mut call_result = std::mem::MaybeUninit::<c_void>::uninit();

        libffi::raw::ffi_call(
            &mut self.cif as *mut _,
            Some(*code_ptr.as_safe_fun()),
            call_result.as_mut_ptr(),
            args.as_mut_ptr(),
        );

        for closure in closures {
            closure_free(closure);
        }

        call_result.assume_init_mut()
    }
}

struct ClosureUserData<'vm> {
    vm: *mut VM<'vm>,
    func: *const Func,
}

// TODO: closures don't work in multithreaded code right now.
// TODO: this isn't a big issue as compile-time code isn't meant to be very complex.
// TODO: but, this should be fixed.
unsafe extern "C" fn closure_callback(
    _cif: &ffi_cif,
    result: &mut c_void,
    args: *const *const c_void,
    userdata: &ClosureUserData,
) {
    let mut func = (&*userdata.func).clone();
    let arg_count = func.arg_types.len();

    // set up function args
    if arg_count > 0 {
        let args = std::slice::from_raw_parts(args, arg_count);
        for (arg_type, arg) in func.arg_types.iter().zip(args) {
            let value = Value::from_type_and_ptr(arg_type, *arg as RawPointer);
            (&mut *userdata.vm).stack.push(value);
        }
    }

    // we need the VM to Halt instead of Return
    *func.code.instructions.last_mut().unwrap() = Instruction::Halt;

    let value = (&mut *userdata.vm).run_func(func);

    // pop the function args manually
    if arg_count > 0 {
        for _ in 0..arg_count {
            (&mut *userdata.vm).stack.pop();
        }
    }

    (&mut *userdata.vm).frames.pop();

    match value {
        Value::I8(v) => *(result as *mut _ as *mut _) = v,
        Value::I16(v) => *(result as *mut _ as *mut _) = v,
        Value::I32(v) => *(result as *mut _ as *mut _) = v,
        Value::I64(v) => *(result as *mut _ as *mut _) = v,
        Value::Int(v) => *(result as *mut _ as *mut _) = v,
        Value::U8(v) => *(result as *mut _ as *mut _) = v,
        Value::U16(v) => *(result as *mut _ as *mut _) = v,
        Value::U32(v) => *(result as *mut _ as *mut _) = v,
        Value::U64(v) => *(result as *mut _ as *mut _) = v,
        Value::Uint(v) => *(result as *mut _ as *mut _) = v,
        Value::F32(v) => *(result as *mut _ as *mut _) = v,
        Value::F64(v) => *(result as *mut _ as *mut _) = v,
        Value::Bool(v) => *(result as *mut _ as *mut _) = v,
        Value::Aggregate(_) => todo!(),
        Value::Pointer(v) => *(result as *mut _ as *mut _) = v.as_inner_raw(),
        _ => panic!("unexpected value `{}`", value.to_string()),
    }
}

trait AsFfiType {
    unsafe fn as_ffi_type(&self) -> TypePointer;
}

impl AsFfiType for TyKind {
    unsafe fn as_ffi_type(&self) -> TypePointer {
        match self {
            TyKind::Bool => ffi_type!(types::uint8),
            TyKind::Int(ty) => match ty {
                IntTy::I8 => ffi_type!(types::sint8),
                IntTy::I16 => ffi_type!(types::sint16),
                IntTy::I32 => ffi_type!(types::sint32),
                IntTy::I64 => ffi_type!(types::sint64),
                IntTy::Int => {
                    if IS_64BIT {
                        ffi_type!(types::sint64)
                    } else {
                        ffi_type!(types::sint32)
                    }
                }
            },
            TyKind::Uint(ty) => match ty {
                UintTy::U8 => ffi_type!(types::uint8),
                UintTy::U16 => ffi_type!(types::uint16),
                UintTy::U32 => ffi_type!(types::uint32),
                UintTy::U64 => ffi_type!(types::uint64),
                UintTy::Uint => {
                    if IS_64BIT {
                        ffi_type!(types::uint64)
                    } else {
                        ffi_type!(types::uint32)
                    }
                }
            },
            TyKind::Float(ty) => match ty {
                FloatTy::F16 | FloatTy::F32 => ffi_type!(types::float),
                FloatTy::F64 => ffi_type!(types::double),
                FloatTy::Float => {
                    if IS_64BIT {
                        ffi_type!(types::double)
                    } else {
                        ffi_type!(types::float)
                    }
                }
            },
            TyKind::Unit
            | TyKind::Pointer(_, _)
            | TyKind::MultiPointer(_, _)
            | TyKind::Fn(_)
            | TyKind::Array(_, _) => {
                ffi_type!(types::pointer)
            }
            TyKind::Slice(_, _) => todo!(),
            TyKind::Tuple(tuple_elements) => {
                let size = self.size_of(WORD_SIZE);
                let align = self.align_of(WORD_SIZE);

                let mut elements: Vec<TypePointer> = vec![];
                for el in tuple_elements.iter() {
                    elements.push(el.as_ffi_type());
                }

                let elements_ptr = elements.as_mut_ptr();

                ffi_type!(*Box::new(ffi_type {
                    size,
                    alignment: align as u16,
                    type_: type_tag::STRUCT,
                    elements: elements_ptr,
                }))
            }
            TyKind::Struct(st) => {
                let size = st.size_of(WORD_SIZE);
                let align = st.align_of(WORD_SIZE);

                let mut elements: Vec<TypePointer> = vec![];
                for field in st.fields.iter() {
                    elements.push(field.ty.as_ffi_type());
                }

                let elements_ptr = elements.as_mut_ptr();

                ffi_type!(*Box::new(ffi_type {
                    size,
                    alignment: align as u16,
                    type_: type_tag::STRUCT,
                    elements: elements_ptr,
                }))
            }
            TyKind::Infer(_, ty) => match ty {
                InferTy::AnyInt => ffi_type!(types::sint64),
                InferTy::AnyFloat => ffi_type!(types::float),
                InferTy::PartialStruct(_) => todo!(),
                InferTy::PartialTuple(_) => todo!(),
            },
            TyKind::Never => ffi_type!(types::void),
            _ => panic!("invalid type {}", self),
        }
    }
}

// trait AsFfiArg {
//     unsafe fn as_ffi_arg(&mut self, size: usize, alignement: usize, ffi: &mut Ffi) -> RawPointer;
// }

// impl AsFfiArg for Value {
//     unsafe fn as_ffi_arg(&mut self, size: usize, alignment: usize, ffi: &mut Ffi) -> RawPointer {
//         match self {
//             Value::I8(v) => raw_ptr!(v),
//             Value::I16(v) => raw_ptr!(v),
//             Value::I32(v) => raw_ptr!(v),
//             Value::I64(v) => raw_ptr!(v),
//             Value::Int(v) => raw_ptr!(v),
//             Value::U8(v) => raw_ptr!(v),
//             Value::U16(v) => raw_ptr!(v),
//             Value::U32(v) => raw_ptr!(v),
//             Value::U64(v) => raw_ptr!(v),
//             Value::Uint(v) => raw_ptr!(v),
//             Value::Bool(v) => raw_ptr!(v),
//             Value::F32(v) => raw_ptr!(v),
//             Value::F64(v) => raw_ptr!(v),
//             Value::Aggregate(v) => {
//                 let mut bytes = BytesMut::with_capacity(size);

//                 for value in v.iter() {
//                     put_value(&mut bytes, value);

//                     // TODO: this could be more efficient
//                     let value_size = (*value.get_ty_kind().as_ffi_type()).size;
//                     if value_size < alignment {
//                         let padding = alignment - value_size;
//                         bytes.put_bytes(0, padding);
//                     }
//                 }

//                 bytes.as_mut_ptr() as RawPointer
//             }
//             Value::Pointer(ptr) => raw_ptr!(ptr.as_raw()),
//             Value::Func(func) => {
//                 let (closure, code_ptr) = closure_alloc();

//                 // ffi.closures.push(Closure {
//                 //     func: func as _,
//                 //     closure,
//                 //     code_ptr,
//                 // });

//                 // TODO: use func's return type
//                 // TODO: use func's param types
//                 let mut function = Box::new(Function::new(&[], &TyKind::Int(IntTy::I32)));

//                 prep_closure(
//                     closure,
//                     &mut function.cif as _,
//                     closure_callback,
//                     std::ptr::null(),
//                     code_ptr,
//                 )
//                 .unwrap();

//                 raw_ptr!(function.closure.unwrap().1.as_mut_ptr())
//                 // code_ptr.as_mut_ptr()
//             }
//             Value::ForeignFunc(func) => {
//                 todo!()
//                 // let symbol = ffi.load_symbol(func.lib_path, func.name);
//                 // raw_ptr!(*symbol)
//             }
//             _ => panic!("can't pass `{}` through ffi", self.to_string()),
//         }
//     }
// }
