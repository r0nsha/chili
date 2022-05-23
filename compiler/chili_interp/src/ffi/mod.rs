use crate::{
    vm::{
        byte_seq::{ByteSeq, PutValue},
        instruction::Instruction,
        value::{ForeignFunc, Func, Value},
        VM,
    },
    IS_64BIT, WORD_SIZE,
};
use bumpalo::Bump;
use chili_ast::ty::{align::AlignOf, size::SizeOf, *};
use libffi::{
    low::{ffi_cif, CodePtr},
    middle::{Cif, Closure, Type},
};
use std::{collections::HashMap, ffi::c_void, path::Path};
use ustr::{ustr, Ustr, UstrMap};

macro_rules! raw_ptr {
    ($value: expr) => {
        $value as *mut _ as RawPointer
    };
}

pub type RawPointer = *mut c_void;

pub(crate) struct Ffi {
    libs: UstrMap<libloading::Library>,
    symbols: HashMap<(Ustr, Ustr), *mut c_void>,
    libc: Ustr,
}

fn find_libc() -> String {
    let libc_file_name = match std::env::consts::OS {
        "windows" => return "msvcrt".to_string(),
        _ => "libc.so.6",
    };

    fn exists(path: String) -> Option<String> {
        if Path::new(&path).exists() {
            Some(path)
        } else {
            None
        }
    }

    exists(format!("/lib/{}", libc_file_name))
        .or_else(|| exists(format!("/usr/lib/{}", libc_file_name)))
        .or_else(|| exists(format!("/lib/x86_64-linux-gnu/{}", libc_file_name)))
        .unwrap_or_else(|| {
            panic!("couldn't find libc on the current machine. this is most likely an ICE")
        })
}

impl Ffi {
    pub(crate) fn new() -> Self {
        Self {
            libs: Default::default(),
            symbols: Default::default(),
            libc: ustr(&find_libc()),
        }
    }

    pub(crate) unsafe fn load_symbol(&mut self, lib_path: Ustr, name: Ustr) -> &mut *mut c_void {
        self.symbols.entry((lib_path, name)).or_insert_with(|| {
            let lib_name = match lib_path.as_str() {
                "c" | "C" => self.libc,
                _ => lib_path,
            };

            let lib = self
                .libs
                .entry(lib_name)
                .or_insert_with(|| libloading::Library::new(lib_name.as_str()).unwrap());

            *lib.get(name.as_bytes()).unwrap()
        })
    }

    pub(crate) unsafe fn call(
        &mut self,
        vm: *mut VM,
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

        Value::from_type_and_ptr(&func.return_ty, result as RawPointer)
    }
}

#[derive(Debug)]
struct Function {
    cif: Cif,
    arg_types: Vec<TyKind>,
}

impl Function {
    unsafe fn new(arg_types: &[TyKind], return_type: &TyKind) -> Self {
        let cif_return_type: Type = return_type.as_ffi_type();
        let cif_arg_types: Vec<Type> = arg_types.iter().map(|arg| arg.as_ffi_type()).collect();
        let cif = Cif::new(cif_arg_types, cif_return_type);

        Self {
            cif,
            arg_types: arg_types.to_vec(),
        }
    }

    unsafe fn new_variadic(
        arg_types: &[TyKind],
        variadic_arg_types: &[TyKind],
        return_type: &TyKind,
    ) -> Self {
        let cif_return_type: Type = return_type.as_ffi_type();
        let arg_types: Vec<TyKind> = arg_types
            .iter()
            .chain(variadic_arg_types.iter())
            .cloned()
            .collect();
        let cif_arg_types: Vec<Type> = arg_types.iter().map(|arg| arg.as_ffi_type()).collect();
        let cif = Cif::new_variadic(cif_arg_types, arg_types.len(), cif_return_type);

        Self {
            cif,
            arg_types: arg_types.to_vec(),
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
        let bump = Bump::with_capacity(arg_values.len() * 2);

        for (arg, arg_type) in arg_values.iter_mut().zip(self.arg_types.iter()) {
            let size = arg_type.size_of(WORD_SIZE);
            let alignment = arg_type.align_of(WORD_SIZE);

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
                    let mut bytes = ByteSeq::new(size);

                    let mut offset = 0;

                    for value in v.elements.iter() {
                        bytes.offset_mut(offset).put_value(value);
                        offset += alignment;
                    }

                    bytes.as_mut_ptr() as RawPointer
                }
                Value::Array(v) => raw_ptr!(&mut v.bytes.as_mut_ptr()),
                Value::Pointer(ptr) => raw_ptr!(ptr.as_raw()),
                Value::Func(func) => {
                    let function = Function::new(&func.arg_types, &func.return_type);

                    let user_data = bump.alloc(ClosureUserData { vm, func });

                    let closure =
                        bump.alloc(Closure::new(function.cif, closure_callback, user_data));

                    let code_ptr =
                        closure.instantiate_code_ptr::<c_void>() as *const c_void as *mut c_void;

                    raw_ptr!(code_ptr)
                }
                Value::ForeignFunc(func) => {
                    let symbol = ffi.load_symbol(func.lib_path, func.name);
                    raw_ptr!(symbol)
                }
                _ => panic!("can't pass `{}` through ffi", arg.to_string()),
            };

            args.push(arg_ptr);
        }

        let mut call_result = std::mem::MaybeUninit::<c_void>::uninit();

        libffi::raw::ffi_call(
            self.cif.as_raw_ptr(),
            Some(*code_ptr.as_safe_fun()),
            call_result.as_mut_ptr(),
            args.as_mut_ptr(),
        );

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
            (*userdata.vm).stack.push(value);
        }
    }

    // we need the VM to Halt instead of Return
    *func.code.instructions.last_mut().unwrap() = Instruction::Halt;

    let value = (*userdata.vm).run_func(func);

    // pop the function args manually
    if arg_count > 0 {
        for _ in 0..arg_count {
            (*userdata.vm).stack.pop();
        }
    }

    // emulate a return, popping the current frame, and setting the last one
    (*userdata.vm).frames.pop();
    (*userdata.vm).frame = (*userdata.vm).frames.last_mut() as _;

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
    unsafe fn as_ffi_type(&self) -> Type;
}

impl AsFfiType for TyKind {
    unsafe fn as_ffi_type(&self) -> Type {
        match self {
            TyKind::Bool => Type::u8(),
            TyKind::Int(ty) => match ty {
                IntTy::I8 => Type::i8(),
                IntTy::I16 => Type::i16(),
                IntTy::I32 => Type::i32(),
                IntTy::I64 => Type::i64(),
                IntTy::Int => {
                    if IS_64BIT {
                        Type::i64()
                    } else {
                        Type::i32()
                    }
                }
            },
            TyKind::Uint(ty) => match ty {
                UintTy::U8 => Type::u8(),
                UintTy::U16 => Type::u16(),
                UintTy::U32 => Type::u32(),
                UintTy::U64 => Type::u64(),
                UintTy::Uint => {
                    if IS_64BIT {
                        Type::u64()
                    } else {
                        Type::u32()
                    }
                }
            },
            TyKind::Float(ty) => match ty {
                FloatTy::F16 | FloatTy::F32 => Type::f32(),
                FloatTy::F64 => Type::f64(),
                FloatTy::Float => {
                    if IS_64BIT {
                        Type::f64()
                    } else {
                        Type::f32()
                    }
                }
            },
            TyKind::Unit
            | TyKind::Pointer(_, _)
            | TyKind::MultiPointer(_, _)
            | TyKind::Fn(_)
            | TyKind::Array(_, _) => Type::pointer(),
            TyKind::Slice(_, _) => Type::structure([Type::pointer(), Type::usize()]),
            TyKind::Tuple(tuple_elements) => {
                Type::structure(tuple_elements.iter().map(|ty| ty.as_ffi_type()))
            }
            TyKind::Struct(st) => Type::structure(st.fields.iter().map(|f| f.ty.as_ffi_type())),
            TyKind::Infer(_, ty) => match ty {
                InferTy::AnyInt => {
                    if IS_64BIT {
                        Type::i64()
                    } else {
                        Type::i32()
                    }
                }
                InferTy::AnyFloat => {
                    if IS_64BIT {
                        Type::f64()
                    } else {
                        Type::f32()
                    }
                }
                InferTy::PartialStruct(_) => todo!(),
                InferTy::PartialTuple(_) => todo!(),
            },
            TyKind::Never => Type::void(),
            _ => panic!("invalid type {}", self),
        }
    }
}
