use super::{
    interp::Interp,
    vm::{
        bytecode::Op,
        value::{ExternFunction, Function, FunctionValue, Value},
        VM,
    },
    IS_64BIT,
};
use crate::types::*;
use bumpalo::Bump;
use libffi::{
    low::{ffi_cif, CodePtr},
    middle::{Cif, Closure, Type as FfiType},
};
use std::{collections::HashMap, ffi::c_void, path::Path};
use ustr::{ustr, Ustr, UstrMap};

macro_rules! raw_ptr {
    ($value: expr) => {
        $value as *mut _ as RawPointer
    };
}

pub type RawPointer = *mut c_void;

pub struct Ffi {
    libs: UstrMap<libloading::Library>,
    symbols: HashMap<(Ustr, Ustr), RawPointer>,
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
        .unwrap_or_else(|| panic!("couldn't find libc on the current machine. this is most likely an ICE"))
}

impl Ffi {
    pub fn new() -> Self {
        Self {
            libs: Default::default(),
            symbols: Default::default(),
            libc: ustr(&find_libc()),
        }
    }

    pub unsafe fn load_symbol(&mut self, lib_path: Ustr, name: Ustr) -> &mut RawPointer {
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

    pub unsafe fn call(
        &mut self,
        function: ExternFunction,
        mut args: Vec<Value>,
        vm: *mut VM,
        interp: *const Interp,
    ) -> Value {
        let symbol = self.load_symbol(function.lib_path, function.name);

        let function_type = &function.ty;
        let param_types = function_type.params.iter().map(|p| p.ty.clone()).collect::<Vec<Type>>();

        let mut function = if function_type.is_variadic() {
            let variadic_arg_types: Vec<Type> = args
                .iter()
                .skip(function_type.params.len())
                .map(|value| value.get_type(&*interp))
                .collect();

            FfiFunction::new_variadic(&param_types, &variadic_arg_types, &function_type.return_type)
        } else {
            FfiFunction::new(&param_types, &function_type.return_type)
        };

        let result = function.call(*symbol, &mut args, self, vm);

        Value::from_type_and_ptr(&function_type.return_type, result as RawPointer)
    }
}

#[derive(Debug)]
struct FfiFunction {
    cif: Cif,
}

impl FfiFunction {
    unsafe fn new(arg_types: &[Type], return_type: &Type) -> Self {
        let cif_return_type: FfiType = return_type.as_ffi_type();

        let cif_arg_types: Vec<FfiType> = arg_types.iter().map(|arg| arg.as_ffi_type()).collect();

        let cif = Cif::new(cif_arg_types, cif_return_type);

        Self { cif }
    }

    unsafe fn new_variadic(arg_types: &[Type], variadic_arg_types: &[Type], return_type: &Type) -> Self {
        let cif_return_type: FfiType = return_type.as_ffi_type();

        let arg_types: Vec<Type> = arg_types.iter().chain(variadic_arg_types.iter()).cloned().collect();

        let cif_arg_types: Vec<FfiType> = arg_types.iter().map(|arg| arg.as_ffi_type()).collect();

        let cif = Cif::new_variadic(cif_arg_types, arg_types.len(), cif_return_type);

        Self { cif }
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

        for arg in arg_values.iter_mut() {
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
                Value::Buffer(v) => raw_ptr!(&mut v.bytes.as_mut_ptr()),
                Value::Pointer(ptr) => raw_ptr!(ptr.as_raw()),
                Value::Function(addr) => match (*vm).interp.get_function(addr.id).unwrap() {
                    FunctionValue::Orphan(function) => {
                        let ffi_function = FfiFunction::new(
                            &function.ty.params.iter().map(|p| &p.ty).cloned().collect::<Vec<Type>>(),
                            &function.ty.return_type,
                        );

                        let user_data = bump.alloc(ClosureUserData { vm, function });

                        let closure = bump.alloc(Closure::new(ffi_function.cif, closure_callback, user_data));

                        let code_ptr = closure.instantiate_code_ptr::<c_void>() as *const c_void as RawPointer;

                        raw_ptr!(code_ptr)
                    }
                    FunctionValue::Extern(function) => {
                        let symbol = ffi.load_symbol(function.lib_path, function.name);
                        raw_ptr!(symbol)
                    }
                },
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
    function: *const Function,
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
    let mut func = (&*userdata.function).clone();
    let arg_count = func.ty.params.len();

    // set up function args
    if arg_count > 0 {
        let args = std::slice::from_raw_parts(args, arg_count);
        for (param, arg) in func.ty.params.iter().zip(args) {
            let value = Value::from_type_and_ptr(&param.ty, *arg as RawPointer);
            (*userdata.vm).stack.push(value);
        }
    }

    // we need the VM to Halt instead of Return
    *func.code.as_mut_slice().last_mut().unwrap() = u8::from(Op::Halt);

    let value = (*userdata.vm).run_function(func);

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
        Value::Pointer(v) => *(result as *mut _ as *mut _) = v.as_inner_raw(),
        _ => panic!("unexpected value `{}`", value.to_string()),
    }
}

trait AsFfiType {
    unsafe fn as_ffi_type(&self) -> FfiType;
}

impl AsFfiType for Type {
    unsafe fn as_ffi_type(&self) -> FfiType {
        match self {
            Type::Bool => FfiType::u8(),
            Type::Int(ty) => match ty {
                IntType::I8 => FfiType::i8(),
                IntType::I16 => FfiType::i16(),
                IntType::I32 => FfiType::i32(),
                IntType::I64 => FfiType::i64(),
                IntType::Int => {
                    if IS_64BIT {
                        FfiType::i64()
                    } else {
                        FfiType::i32()
                    }
                }
            },
            Type::Uint(ty) => match ty {
                UintType::U8 => FfiType::u8(),
                UintType::U16 => FfiType::u16(),
                UintType::U32 => FfiType::u32(),
                UintType::U64 => FfiType::u64(),
                UintType::Uint => {
                    if IS_64BIT {
                        FfiType::u64()
                    } else {
                        FfiType::u32()
                    }
                }
            },
            Type::Float(ty) => match ty {
                FloatType::F16 | FloatType::F32 => FfiType::f32(),
                FloatType::F64 => FfiType::f64(),
                FloatType::Float => {
                    if IS_64BIT {
                        FfiType::f64()
                    } else {
                        FfiType::f32()
                    }
                }
            },
            Type::Unit | Type::Pointer(_, _) | Type::Function(_) | Type::Array(_, _) => FfiType::pointer(),
            Type::Slice(_) => FfiType::structure([FfiType::pointer(), FfiType::usize()]),
            Type::Tuple(tuple_elements) => FfiType::structure(tuple_elements.iter().map(|ty| ty.as_ffi_type())),
            Type::Struct(st) => FfiType::structure(st.fields.iter().map(|f| f.ty.as_ffi_type())),
            Type::Infer(_, ty) => match ty {
                InferType::AnyInt => {
                    if IS_64BIT {
                        FfiType::i64()
                    } else {
                        FfiType::i32()
                    }
                }
                InferType::AnyFloat => {
                    if IS_64BIT {
                        FfiType::f64()
                    } else {
                        FfiType::f32()
                    }
                }
                InferType::PartialStruct(_) => todo!(),
                InferType::PartialTuple(_) => todo!(),
            },
            Type::Never => FfiType::void(),
            _ => panic!("invalid type {}", self),
        }
    }
}
