use crate::{
    value::{ForeignFunc, Value},
    IS_64BIT, WORD_SIZE,
};
use bytes::{BufMut, BytesMut};
use chili_ast::ty::{align::AlignOf, size::SizeOf, *};
use libffi::{
    low::{
        ffi_abi_FFI_DEFAULT_ABI as ABI, ffi_cif, ffi_type, prep_cif, prep_cif_var, type_tag, types,
        CodePtr,
    },
    raw::FFI_TYPE_STRUCT,
};
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

    pub(crate) unsafe fn call(&mut self, func: ForeignFunc, mut args: Vec<Value>) -> Value {
        // return self.test();
        let lib = self.load_lib(func.lib_path);
        let symbol = lib.get::<&mut c_void>(func.name.as_bytes()).unwrap();

        let mut cif = ffi_cif::default();

        let return_type = func.ret_ty.as_ffi_type();

        let mut arg_types: Vec<TypePointer> = Vec::with_capacity(args.len());

        for param in func.param_tys.iter() {
            arg_types.push(param.as_ffi_type());
        }

        if func.variadic {
            for arg in args.iter().skip(func.param_tys.len()) {
                // arg_types.push(arg.as_ffi_type());
                arg_types.push(arg.get_ty_kind().as_ffi_type());
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

        let mut call_args: Vec<RawPointer> = Vec::with_capacity(args.len());

        for (arg, arg_type) in args.iter_mut().zip(arg_types) {
            let arg_type = *arg_type;
            let ptr = arg.as_ffi_arg(arg_type.size, arg_type.alignment.into());
            call_args.push(ptr);
        }

        let mut call_result = std::mem::MaybeUninit::<c_void>::uninit();

        libffi::raw::ffi_call(
            &mut cif as *mut _,
            Some(*code_ptr.as_safe_fun()),
            call_result.as_mut_ptr(),
            call_args.as_mut_ptr(),
        );

        let call_result = call_result.assume_init_mut();

        Value::from_type_and_ptr(&func.ret_ty, call_result as RawPointer as RawPointer)
    }

    // pub(crate) unsafe fn test(&mut self) -> Value {
    //     let lib = self.load_lib(ustr("msvcrt"));
    //     let symbol = lib.get::<&mut c_void>(b"printf").unwrap();

    //     let mut cif = ffi_cif::default();
    //     let return_type = ffi_type!(types::sint64);
    //     let mut arg_types = vec![ffi_type!(types::pointer), ffi_type!(types::sint64)];
    //     prep_cif_var(&mut cif, ABI, 1, 2, return_type, arg_types.as_mut_ptr()).unwrap();

    //     let code_ptr = CodePtr::from_ptr(*symbol);

    //     let s = ustr("hello %d\n");
    //     let mut p = s.as_char_ptr() as RawPointer;
    //     let mut d: i64 = 42;
    //     let mut call_args = vec![raw_ptr!(&mut p), raw_ptr!(&mut d)];

    //     let mut call_result = std::mem::MaybeUninit::<c_void>::uninit();
    //     libffi::raw::ffi_call(
    //         &mut cif as *mut _,
    //         Some(*code_ptr.as_safe_fun()),
    //         call_result.as_mut_ptr(),
    //         call_args.as_mut_ptr(),
    //     );

    //     Value::Uint(0)
    // }
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
            TyKind::Unit | TyKind::Pointer(_, _) | TyKind::MultiPointer(_, _) => {
                ffi_type!(types::pointer)
            }
            TyKind::Fn(_) => todo!(),
            TyKind::Array(_, _) => todo!(),
            TyKind::Slice(_, _) => todo!(),
            TyKind::Tuple(_) => todo!(),
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

trait AsFfiArg {
    unsafe fn as_ffi_arg(&mut self, size: usize, alignement: usize) -> RawPointer;
}

impl AsFfiArg for Value {
    unsafe fn as_ffi_arg(&mut self, size: usize, alignment: usize) -> RawPointer {
        match self {
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
                let mut bytes = BytesMut::with_capacity(size);

                for value in v.iter() {
                    put_value(&mut bytes, value);

                    // TODO: this could be more efficient
                    // let value_size = (*value.as_ffi_type()).size;
                    // if value_size < alignment {
                    //     let padding = alignment - value_size;
                    //     bytes.put_bytes(0, padding);
                    // }
                }

                bytes.as_mut_ptr() as RawPointer
            }
            Value::Pointer(ptr) => raw_ptr!(ptr.as_raw()),
            Value::Func(_) => todo!("func"),
            Value::ForeignFunc(_) => todo!("foreign func"),
            _ => panic!("can't pass `{}` through ffi", self.to_string()),
        }
    }
}

// Note (Ron): Important - This function WILL fail in Big Endian systems!!
// Note (Ron): This isn't very crucial, since the most common systems are little endian - but this needs to be fixed anyway.
fn put_value(bytes: &mut BytesMut, value: &Value) {
    match value {
        Value::I8(v) => bytes.put_i8(*v),
        Value::I16(v) => bytes.put_i16_le(*v),
        Value::I32(v) => bytes.put_i32_le(*v),
        Value::I64(v) => bytes.put_i64(*v),
        Value::Int(v) => {
            if IS_64BIT {
                bytes.put_i64_le(*v as i64)
            } else {
                bytes.put_i32_le(*v as i32)
            }
        }
        Value::U8(v) => bytes.put_u8(*v),
        Value::U16(v) => bytes.put_u16_le(*v),
        Value::U32(v) => bytes.put_u32_le(*v),
        Value::U64(v) => bytes.put_u64_le(*v),
        Value::Uint(v) => {
            if IS_64BIT {
                bytes.put_u64_le(*v as u64)
            } else {
                bytes.put_u32_le(*v as u32)
            }
        }
        Value::F32(v) => bytes.put_f32_le(*v),
        Value::F64(v) => bytes.put_f64_le(*v),
        Value::Bool(v) => bytes.put_uint_le(*v as u64, 1),
        Value::Aggregate(v) => {
            // TODO: need to include struct padding here
            for value in v {
                put_value(bytes, value)
            }
        }
        Value::Pointer(v) => bytes.put_u64_le(v.as_inner_raw() as u64),
        Value::Func(v) => todo!(),
        _ => panic!("can't convert `{}` to raw bytes", value.to_string()),
    }
}
