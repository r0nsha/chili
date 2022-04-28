use crate::value::{ForeignFunc, Value};
use chili_ast::ty::*;
use libffi::low::{
    ffi_abi_FFI_DEFAULT_ABI, ffi_cif, ffi_type, prep_cif, prep_cif_var, types, CodePtr,
};
use std::{collections::HashMap, ffi::c_void, mem};
use ustr::{ustr, Ustr};

const IS_64BIT: bool = mem::size_of::<usize>() == 8;

pub(crate) struct Ffi {
    libs: HashMap<Ustr, libloading::Library>,
}

impl Ffi {
    pub(crate) fn new() -> Self {
        Self {
            libs: HashMap::new(),
        }
    }

    pub(crate) unsafe fn get_or_load_lib(&mut self, lib_path: Ustr) -> &libloading::Library {
        // TODO: default libc and file extension should depend on the current platform
        let lib_name = match lib_path.as_str() {
            "c" | "C" | "libucrt" => ustr("msvcrt"),
            _ => lib_path,
        };

        self.libs
            .entry(lib_name)
            .or_insert_with(|| libloading::Library::new(format!("{}.dll", lib_name)).unwrap())
    }

    pub(crate) unsafe fn call(&mut self, func: ForeignFunc, mut args: Vec<Value>) -> Value {
        let lib = self.get_or_load_lib(func.lib_path);

        let symbol = lib.get::<&mut c_void>(func.name.as_bytes()).unwrap();

        let mut cif = ffi_cif::default();

        let return_type = &mut func.ret_ty.as_ffi_type() as *mut ffi_type;

        let mut arg_types: Vec<*mut ffi_type> = func
            .param_tys
            .iter()
            .map(|param| &mut param.as_ffi_type() as *mut _)
            .collect();

        if func.variadic {
            for arg in args.iter().skip(arg_types.len()) {
                arg_types.push(&mut arg.as_ffi_type() as *mut _);
            }

            prep_cif_var(
                &mut cif,
                ffi_abi_FFI_DEFAULT_ABI,
                func.param_tys.len(),
                arg_types.len(),
                return_type,
                arg_types.as_mut_ptr(),
            )
            .unwrap()
        } else {
            prep_cif(
                &mut cif,
                ffi_abi_FFI_DEFAULT_ABI,
                arg_types.len(),
                return_type,
                arg_types.as_mut_ptr(),
            )
            .unwrap()
        }

        let code_ptr = CodePtr::from_ptr(*symbol);

        println!("{:?}", args);

        let mut args = args
            .iter_mut()
            .map(|arg| arg.as_ffi_arg())
            .collect::<Vec<*mut c_void>>();

        let mut result = mem::MaybeUninit::<c_void>::uninit();
        println!("1");
        libffi::raw::ffi_call(
            &mut cif as *mut _,
            Some(*code_ptr.as_safe_fun()),
            result.as_mut_ptr(),
            args.as_mut_ptr(),
        );
        println!("2");

        let call_result = result.assume_init_mut();

        Value::from_ptr(&func.ret_ty, call_result as *mut c_void as *mut u8)
    }
}

trait AsFfiType {
    unsafe fn as_ffi_type(&self) -> ffi_type;
}

impl AsFfiType for TyKind {
    unsafe fn as_ffi_type(&self) -> ffi_type {
        match self {
            TyKind::Bool => types::uint8,
            TyKind::Int(ty) => match ty {
                IntTy::I8 => types::sint8,
                IntTy::I16 => types::sint16,
                IntTy::I32 => types::sint32,
                IntTy::I64 => types::sint64,
                IntTy::Int => {
                    if IS_64BIT {
                        types::sint64
                    } else {
                        types::sint32
                    }
                }
            },
            TyKind::UInt(ty) => match ty {
                UIntTy::U8 => types::uint8,
                UIntTy::U16 => types::uint16,
                UIntTy::U32 => types::uint32,
                UIntTy::U64 => types::uint64,
                UIntTy::UInt => {
                    if IS_64BIT {
                        types::uint64
                    } else {
                        types::uint32
                    }
                }
            },
            TyKind::Float(ty) => match ty {
                FloatTy::F16 | FloatTy::F32 => types::float,
                FloatTy::F64 => types::double,
                FloatTy::Float => {
                    if IS_64BIT {
                        types::double
                    } else {
                        types::float
                    }
                }
            },
            TyKind::Unit | TyKind::Pointer(_, _) | TyKind::MultiPointer(_, _) => types::pointer,
            TyKind::Fn(_) => todo!(),
            TyKind::Array(_, _) => todo!(),
            TyKind::Slice(_, _) => todo!(),
            TyKind::Tuple(_) => todo!(),
            TyKind::Struct(_) => todo!(),
            TyKind::Infer(_, ty) => match ty {
                InferTy::AnyInt => types::sint64,
                InferTy::AnyFloat => types::float,
                InferTy::PartialStruct(_) => todo!(),
                InferTy::PartialTuple(_) => todo!(),
            },
            TyKind::Never => types::void,
            _ => panic!("invalid type {}", self),
        }
    }
}

impl AsFfiType for Value {
    unsafe fn as_ffi_type(&self) -> ffi_type {
        match self {
            Value::I8(_) => types::sint8,
            Value::I16(_) => types::sint16,
            Value::I32(_) => types::sint32,
            Value::I64(_) => types::sint64,
            Value::Int(_) => {
                if IS_64BIT {
                    types::sint64
                } else {
                    types::sint32
                }
            }
            Value::U8(_) => types::uint8,
            Value::U16(_) => types::uint16,
            Value::U32(_) => types::uint32,
            Value::U64(_) => types::uint64,
            Value::UInt(_) => {
                if IS_64BIT {
                    types::uint64
                } else {
                    types::uint32
                }
            }
            Value::F32(_) => types::float,
            Value::F64(_) => types::double,
            Value::Bool(_) => types::uint8,
            Value::Tuple(_) => todo!(),
            Value::Ptr(..) => types::pointer,
            Value::Slice(_) => todo!(),
            Value::Func(_) => todo!(),
            Value::ForeignFunc(_) => todo!(),
        }
    }
}

macro_rules! raw_ptr {
    ($value: expr) => {
        $value as *mut _ as *mut c_void
    };
}

trait AsFfiArg {
    unsafe fn as_ffi_arg(&mut self) -> *mut c_void;
}

impl AsFfiArg for Value {
    unsafe fn as_ffi_arg(&mut self) -> *mut c_void {
        match self {
            Value::I8(ref mut v) => raw_ptr!(v),
            Value::I16(ref mut v) => raw_ptr!(v),
            Value::I32(ref mut v) => raw_ptr!(v),
            Value::I64(ref mut v) => raw_ptr!(v),
            Value::Int(ref mut v) => raw_ptr!(v),
            Value::U8(ref mut v) => raw_ptr!(v),
            Value::U16(ref mut v) => raw_ptr!(v),
            Value::U32(ref mut v) => raw_ptr!(v),
            Value::U64(ref mut v) => raw_ptr!(v),
            Value::UInt(ref mut v) => raw_ptr!(v),
            Value::Bool(ref mut v) => raw_ptr!(v),
            Value::F32(ref mut v) => raw_ptr!(v),
            Value::F64(ref mut v) => raw_ptr!(v),
            Value::Tuple(_) => todo!("tuple"),
            Value::Ptr(ptr) => raw_ptr!(ptr.as_raw()),
            Value::Slice(_) => todo!("slice"),
            Value::Func(_) => todo!("func"),
            Value::ForeignFunc(_) => todo!("foreign func"),
        }
    }
}
