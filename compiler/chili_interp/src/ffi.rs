use crate::value::{ForeignFunc, Value, ValuePtr};
use chili_ast::ty::*;
use libffi::low::{
    ffi_abi_FFI_DEFAULT_ABI, ffi_cif, ffi_type, prep_cif, prep_cif_var, types, CodePtr,
};
use std::{collections::HashMap, ffi::c_void, mem};
use ustr::{ustr, Ustr};

const IS_64BIT: bool = mem::size_of::<usize>() == 8;

struct Function {
    ptr: *mut c_void,
}

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

    pub(crate) unsafe fn call(&mut self, func: ForeignFunc, args: Vec<Value>) -> Value {
        let lib = self.get_or_load_lib(func.lib_path);

        let symbol = lib.get::<&mut c_void>(func.name.as_bytes()).unwrap();

        let mut cif = ffi_cif::default();

        let ret_ty: *mut ffi_type = &mut func.ret_ty.as_ffi_type();

        let mut param_tys: Vec<*mut ffi_type> = func
            .param_tys
            .iter()
            .map(|param| &mut param.as_ffi_type() as *mut _)
            .collect();

        if func.variadic {
            for arg in args.iter().skip(param_tys.len()) {
                param_tys.push(&mut arg.as_ffi_type() as *mut _);
            }

            prep_cif_var(
                &mut cif,
                ffi_abi_FFI_DEFAULT_ABI,
                param_tys.len(),
                args.len(),
                ret_ty,
                param_tys.as_mut_ptr(),
            )
            .unwrap()
        } else {
            prep_cif(
                &mut cif,
                ffi_abi_FFI_DEFAULT_ABI,
                param_tys.len(),
                ret_ty,
                param_tys.as_mut_ptr(),
            )
            .unwrap()
        }

        let code_ptr = CodePtr::from_ptr(*symbol);
        let args = args
            .iter()
            .map(|arg| unsafe { arg.as_ffi_arg() })
            .collect::<Vec<*mut c_void>>()
            .as_mut_ptr();

        let mut result = mem::MaybeUninit::<c_void>::uninit();

        libffi::raw::ffi_call(
            &mut cif as *mut _,
            Some(*code_ptr.as_safe_fun()),
            result.as_mut_ptr() as *mut c_void,
            args,
        );

        let call_result = result.assume_init_mut();

        Value::Ptr(ValuePtr::from_ptr(
            &func.ret_ty,
            call_result as *mut c_void as *mut u8,
        ))
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
            TyKind::Float(_) => types::float,
            TyKind::Unit => todo!(),
            TyKind::Pointer(_, _) | TyKind::MultiPointer(_, _) => types::pointer,
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
            Value::F32(_) | Value::F64(_) => types::float,
            Value::Bool(_) => types::uint8,
            Value::Tuple(_) => todo!(),
            Value::Ptr(..) => types::pointer,
            Value::Slice(_) => todo!(),
            Value::Func(_) => todo!(),
            Value::ForeignFunc(_) => todo!(),
        }
    }
}

trait AsFfiArg {
    unsafe fn as_ffi_arg(&self) -> *mut c_void;
}

impl AsFfiArg for Value {
    unsafe fn as_ffi_arg(&self) -> *mut c_void {
        match self {
            Value::I8(mut v) => &mut v as *mut _ as *mut c_void,
            Value::I16(mut v) => &mut v as *mut _ as *mut c_void,
            Value::I32(mut v) => &mut v as *mut _ as *mut c_void,
            Value::I64(mut v) => &mut v as *mut _ as *mut c_void,
            Value::Int(mut v) => &mut v as *mut _ as *mut c_void,
            Value::U8(mut v) => &mut v as *mut _ as *mut c_void,
            Value::U16(mut v) => &mut v as *mut _ as *mut c_void,
            Value::U32(mut v) => &mut v as *mut _ as *mut c_void,
            Value::U64(mut v) => &mut v as *mut _ as *mut c_void,
            Value::UInt(mut v) => &mut v as *mut _ as *mut c_void,
            Value::Bool(mut v) => &mut v as *mut _ as *mut c_void,
            Value::F32(mut v) => &mut v as *mut _ as *mut c_void,
            Value::F64(mut v) => &mut v as *mut _ as *mut c_void,
            Value::Tuple(_) => todo!("tuple"),
            Value::Ptr(ptr) => ptr.as_raw() as *mut c_void,
            Value::Slice(_) => todo!("slice"),
            Value::Func(_) => todo!("func"),
            Value::ForeignFunc(_) => todo!("foreign func"),
        }
    }
}
