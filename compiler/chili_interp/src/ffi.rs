use crate::{
    value::{ForeignFunc, Value},
    IS_64BIT, WORD_SIZE,
};
use bytes::{BufMut, BytesMut};
use chili_ast::ty::{align::AlignOf, size::SizeOf, *};
use libffi::{
    low::{
        ffi_abi_FFI_DEFAULT_ABI as ABI, ffi_cif, ffi_type, prep_cif, prep_cif_var, types, CodePtr,
    },
    raw::FFI_TYPE_STRUCT,
};
use std::ffi::c_void;
use ustr::{ustr, Ustr, UstrMap};

macro_rules! raw_ptr {
    ($value: expr) => {
        $value as *mut _ as *mut c_void
    };
}

macro_rules! ffi_type {
    ($value: expr) => {
        &mut $value as *mut ffi_type
    };
}

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

    pub(crate) unsafe fn call(&mut self, func: ForeignFunc, mut args: Vec<Value>) -> Value {
        let lib = self.load_lib(func.lib_path);
        let symbol = lib.get::<&mut c_void>(func.name.as_bytes()).unwrap();

        let mut cif = ffi_cif::default();

        let return_type = ffi_type!(func.ret_ty.as_ffi_type());

        let mut arg_types: Vec<*mut ffi_type> = vec![];

        for param in func.param_tys.iter() {
            arg_types.push(ffi_type!(param.as_ffi_type()));
        }

        // println!(
        //     "{:?} {:?} {:?}",
        //     args[0],
        //     args[0].as_pointer().as_inner_raw(),
        //     *args[0].as_pointer().as_inner_raw()
        // );

        if func.variadic {
            for arg in args.iter().skip(func.param_tys.len()) {
                arg_types.push(ffi_type!(arg.as_ffi_type()));
            }

            prep_cif_var(
                &mut cif,
                ABI,
                func.param_tys.len(),
                arg_types.len(),
                return_type,
                arg_types.as_mut_ptr(),
            )
            .unwrap();
        } else {
            prep_cif(
                &mut cif,
                ABI,
                arg_types.len(),
                return_type,
                arg_types.as_mut_ptr(),
            )
            .unwrap();
        }

        let code_ptr = CodePtr::from_ptr(*symbol);

        let mut call_args: Vec<*mut c_void> = vec![];

        for (index, arg) in args.iter_mut().enumerate() {
            call_args.push(arg.as_ffi_arg(arg_types[index]));
        }

        let mut call_result = std::mem::MaybeUninit::<c_void>::uninit();

        libffi::raw::ffi_call(
            &mut cif as *mut _,
            Some(*code_ptr.as_safe_fun()),
            call_result.as_mut_ptr(),
            call_args.as_mut_ptr(),
        );

        let call_result = call_result.assume_init_mut();

        Value::from_type_and_ptr(&func.ret_ty, call_result as *mut c_void as *mut u8)
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
            TyKind::Uint(ty) => match ty {
                UintTy::U8 => types::uint8,
                UintTy::U16 => types::uint16,
                UintTy::U32 => types::uint32,
                UintTy::U64 => types::uint64,
                UintTy::Uint => {
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
            TyKind::Struct(st) => {
                let size = st.size_of(WORD_SIZE);
                let align = st.align_of(WORD_SIZE);

                // TODO: Leak
                let mut elements: Vec<*mut ffi_type> = vec![];
                for field in st.fields.iter() {
                    elements.push(ffi_type!(field.ty.as_ffi_type()));
                }
                let elements_ptr = elements.as_mut_ptr();
                std::mem::forget(elements);

                ffi_type {
                    size,
                    alignment: align as u16,
                    type_: FFI_TYPE_STRUCT as u16,
                    elements: elements_ptr,
                }
            }
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
            Value::Uint(_) => {
                if IS_64BIT {
                    types::uint64
                } else {
                    types::uint32
                }
            }
            Value::F32(_) => types::float,
            Value::F64(_) => types::double,
            Value::Bool(_) => types::uint8,
            Value::Aggregate(_) => todo!(),
            Value::Pointer(..) => types::pointer,
            Value::Func(_) => todo!(),
            _ => panic!("can't pass `{}` through ffi", self),
        }
    }
}

trait AsFfiArg {
    unsafe fn as_ffi_arg(&mut self, ft: *mut ffi_type) -> *mut c_void;
}

impl AsFfiArg for Value {
    unsafe fn as_ffi_arg(&mut self, ft: *mut ffi_type) -> *mut c_void {
        let alignment = (*ft).alignment as usize;
        let size = (*ft).size;

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
            Value::Uint(ref mut v) => raw_ptr!(v),
            Value::Bool(ref mut v) => raw_ptr!(v),
            Value::F32(ref mut v) => raw_ptr!(v),
            Value::F64(ref mut v) => raw_ptr!(v),
            Value::Aggregate(ref mut v) => {
                let mut bytes = BytesMut::with_capacity(size);

                for value in v.iter() {
                    match value {
                        Value::I8(v) => bytes.put_i8(*v),
                        Value::I16(v) => bytes.put_i16(*v),
                        Value::I32(v) => bytes.put_i32(*v),
                        Value::I64(v) => bytes.put_i64(*v),
                        Value::Int(v) => {
                            if IS_64BIT {
                                bytes.put_i64(*v as i64)
                            } else {
                                bytes.put_i32(*v as i32)
                            }
                        }
                        Value::U8(v) => bytes.put_u8(*v),
                        Value::U16(v) => bytes.put_u16(*v),
                        Value::U32(v) => bytes.put_u32(*v),
                        Value::U64(v) => bytes.put_u64(*v),
                        Value::Uint(v) => {
                            if IS_64BIT {
                                bytes.put_u64(*v as u64)
                            } else {
                                bytes.put_u32(*v as u32)
                            }
                        }
                        Value::F32(v) => bytes.put_f32(*v),
                        Value::F64(v) => bytes.put_f64(*v),
                        Value::Bool(v) => bytes.put_u8(*v as u8),
                        Value::Aggregate(v) => todo!(),
                        Value::Pointer(v) => todo!(),
                        Value::Func(v) => todo!(),
                        _ => panic!("can't convert `{}` to raw bytes", self),
                    }

                    // TODO: this could be more efficient
                    let value_size = value.as_ffi_type().size;
                    if value_size < alignment {
                        let padding = alignment - value.as_ffi_type().size;
                        bytes.put_bytes(0, padding);
                    }
                }

                bytes.as_mut_ptr() as *mut c_void
            }
            Value::Pointer(ref mut ptr) => {
                raw_ptr!(ptr.as_raw())
            }
            Value::Func(_) => todo!("func"),
            Value::ForeignFunc(_) => todo!("foreign func"),
            _ => panic!("can't pass `{}` through ffi", self),
        }
    }
}
