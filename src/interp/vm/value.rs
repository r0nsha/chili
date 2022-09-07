use super::{
    super::ffi::RawPointer,
    super::{IS_64BIT, WORD_SIZE},
    byte_seq::{ByteSeq, GetValue, PutValue},
    bytecode::Bytecode,
};
use crate::{
    ast::ExternLibrary,
    hir::{
        self,
        const_value::{ConstArray, ConstElement, ConstExternVariable, ConstFunction, ConstValue},
    },
    infer::type_ctx::TypeCtx,
    interp::interp::Interp,
    span::Span,
    types::{
        align_of::AlignOf, offset_of::OffsetOf, size_of::SizeOf, FloatType, FunctionType, InferType, IntType, Type,
        UintType,
    },
};
use byteorder::{NativeEndian, ReadBytesExt, WriteBytesExt};
use indexmap::IndexMap;
use paste::paste;
use std::{fmt::Display, mem, slice, str};
use ustr::{ustr, Ustr};

macro_rules! impl_value {
    ($($variant:ident($ty:ty)) , + $(,)?) => {
        #[derive(PartialEq, Debug, Clone, Copy)]
        pub enum ValueKind {
            $(
                $variant
            ),+
        }

        impl Display for ValueKind {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                write!(f, "{}", match self {
                    $(
                        ValueKind::$variant => String::from(stringify!($variant))
                    ),+
                })
            }
        }

        #[derive(Debug, Clone)]
        pub enum Value {
            $(
                $variant($ty)
            ),+
        }

        #[allow(unused)]
        impl Value {
            pub fn kind(&self) -> ValueKind {
                match self {
                    $(
                        Value::$variant(_) => ValueKind::$variant
                    ),+
                }
            }

            pub fn as_c_ref(&mut self) -> RawPointer {
                match self {
                    $(
                        Self::$variant(v) => v as *mut _ as RawPointer
                    ),+
                }
            }

            pub unsafe fn from_kind_and_ptr(kind: ValueKind, ptr: RawPointer) -> Self {
                match kind {
                    $(
                        ValueKind::$variant => Self::$variant((*(ptr as *mut $ty)).clone())
                    ),+
                }
            }

            paste! {
                $(
                    pub fn [<is_ $variant:snake>](&self) -> bool {
                        match self {
                            Self::$variant(_) => true,
                            _ => false
                        }
                    }
                )+

                $(
                    pub fn [<into_ $variant:snake>](self) -> $ty {
                        match self {
                            Self::$variant(v) => v,
                            _ => panic!("got {}, expected {}", self.to_string(), stringify!($variant))
                        }
                    }
                )+

                $(
                    pub fn [<as_ $variant:snake>](&self) -> &$ty {
                        match self {
                            Self::$variant(v) => v,
                            _ => panic!("got {}, expected {}", self.to_string(), stringify!($variant))
                        }
                    }
                )+

                $(
                    pub fn [<as_ $variant:snake _mut>](&mut self) -> &mut $ty {
                        match self {
                            Self::$variant(v) => v,
                            _ => panic!("got {}, expected {}", self.to_string(), stringify!($variant))
                        }
                    }
                )+
            }
        }

        #[derive(Debug, Clone)]
        pub enum Pointer {
            $(
                $variant(*mut $ty)
            ),+
        }

        impl From<&mut Value> for Pointer {
            fn from(value: &mut Value) -> Self {
                match value {
                    $(
                        Value::$variant(v) => Pointer::$variant(v as _)
                    ),+
                }
            }
        }

        #[allow(unused)]
        impl Pointer {
            pub fn kind(&self) -> ValueKind {
                match self {
                    $(
                        Self::$variant(_) => ValueKind::$variant
                    ),+
                }
            }

            pub unsafe fn as_raw(&mut self) -> *mut RawPointer {
                match self {
                    $(
                        Self::$variant(ref mut v) => mem::transmute::<&mut *mut _, *mut RawPointer>(v)
                    ),+
                }
            }

            pub fn as_inner_raw(&self) -> RawPointer {
                match self {
                    $(
                        Self::$variant(v) => *v as RawPointer
                    ),+
                }
            }

            pub unsafe fn offset(&self, offset: isize) -> Self {
                match self {
                    $(
                        Self::$variant(v) => Self::$variant((*v).offset(offset))
                    ),+
                }
            }

            pub fn is_null(&self) -> bool {
                match self {
                    $(
                        Self::$variant(v) => (*v).is_null()
                    ),+
                }
            }

            pub unsafe fn deref_value(&self) -> Value {
                match self {
                    $(
                        Self::$variant(v) => Value::$variant((**v).clone())
                    ),+
                }
            }

            pub fn from_kind_and_ptr(kind: ValueKind, ptr: RawPointer) -> Self {
                match kind {
                    $(
                        ValueKind::$variant => Pointer::$variant(ptr as _)
                    ),+
                }
            }

            pub unsafe fn print(&self) {
                match self {
                    $(
                        Self::$variant(v) => println!("{:?}", **v)
                    ),+
                }
            }

            paste! {
                $(
                    pub fn [<is_ $variant:snake>](&self) -> bool {
                        match self {
                            Self::$variant(_) => true,
                            _ => false
                        }
                    }
                )+

                $(
                    pub fn [<into_ $variant:snake>](self) -> *mut $ty {
                        match self {
                            Self::$variant(v) => v,
                            _ => panic!("got {}, expected {}", self.to_string(), stringify!($variant))
                        }
                    }
                )+

                $(
                    pub fn [<as_ $variant:snake>](&self) -> &*mut $ty {
                        match self {
                            Self::$variant(v) => v,
                            _ => panic!("got {}, expected {}", self.to_string(), stringify!($variant))
                        }
                    }
                )+

                $(
                    pub fn [<as_ $variant:snake _mut>](&mut self) -> &mut *mut $ty {
                        match self {
                            Self::$variant(v) => v,
                            _ => panic!("got {}, expected {}", self.to_string(), stringify!($variant))
                        }
                    }
                )+
            }
        }
    };
}

impl_value! {
    I8(i8),
    I16(i16),
    I32(i32),
    I64(i64),
    Int(isize),
    U8(u8),
    U16(u16),
    U32(u32),
    U64(u64),
    Uint(usize),
    F32(f32),
    F64(f64),
    Bool(bool),
    Buffer(Buffer),
    Pointer(Pointer),
    Function(FunctionAddress),
    ExternVariable(ExternVariable),
    Intrinsic(IntrinsicFunction),
    Type(Type),
}

impl Default for Value {
    fn default() -> Self {
        Self::I8(0)
    }
}

#[derive(Debug, Clone)]
pub struct Buffer {
    pub bytes: ByteSeq,
    pub ty: Type,
}

impl Buffer {
    pub fn as_slice<T>(&self) -> &[T] {
        let ptr = self.bytes.offset(0).read_uint::<NativeEndian>(WORD_SIZE).unwrap() as *mut T;

        let len = self
            .bytes
            .offset(self.ty.offset_of(1, WORD_SIZE))
            .read_uint::<NativeEndian>(WORD_SIZE)
            .unwrap() as usize;

        unsafe { std::slice::from_raw_parts(ptr, len) }
    }

    pub fn as_str(&self) -> &str {
        let slice = self.as_slice::<u8>();

        // TODO: remove this nul terminator handle hack
        let slice_without_nul = if slice.last().map_or(false, |c| *c == b'\0') {
            &slice[..slice.len() - 1]
        } else {
            slice
        };

        std::str::from_utf8(slice_without_nul).unwrap()
    }

    #[allow(unused)]
    pub fn from_ustr(s: Ustr) -> Self {
        let ty = Type::str_pointer();
        let size = ty.size_of(WORD_SIZE);

        let mut bytes = ByteSeq::new(size);

        bytes
            .offset_mut(0)
            .put_value(&Value::Pointer(Pointer::U8(s.as_char_ptr() as *mut u8)));

        bytes
            .offset_mut(ty.offset_of(1, WORD_SIZE))
            .put_value(&Value::Uint(s.len()));

        Buffer { bytes, ty }
    }

    #[allow(unused)]
    pub fn from_str(s: &mut str) -> Self {
        let ty = Type::str_pointer();
        let size = ty.size_of(WORD_SIZE);

        let mut bytes = ByteSeq::new(size);

        bytes
            .offset_mut(0)
            .put_value(&Value::Pointer(Pointer::U8(s.as_mut_ptr())));

        bytes
            .offset_mut(ty.offset_of(1, WORD_SIZE))
            .put_value(&Value::Uint(s.len()));

        Buffer { bytes, ty }
    }

    pub fn from_str_bytes(s: &mut [u8]) -> Self {
        let ty = Type::str_pointer();
        let size = ty.size_of(WORD_SIZE);

        let mut bytes = ByteSeq::new(size);

        bytes
            .offset_mut(0)
            .put_value(&Value::Pointer(Pointer::U8(s.as_mut_ptr())));

        bytes
            .offset_mut(ty.offset_of(1, WORD_SIZE))
            .put_value(&Value::Uint(s.len()));

        Buffer { bytes, ty }
    }

    pub fn from_values<I: IntoIterator<Item = Value>>(values: I, ty: Type) -> Self {
        let size = ty.size_of(WORD_SIZE);

        let mut bytes = ByteSeq::new(size);

        for (index, value) in values.into_iter().enumerate() {
            let offset = ty.offset_of(index, WORD_SIZE);
            bytes.offset_mut(offset).put_value(&value);
        }

        Self { bytes, ty }
    }

    pub fn get_all_values(&self) -> Vec<Value> {
        match &self.ty {
            Type::Unit | Type::Never => vec![],
            Type::Struct(struct_type) => struct_type
                .fields
                .iter()
                .enumerate()
                .map(|(index, _)| self.get_value_at_index(index))
                .collect(),
            Type::Tuple(elements) => elements
                .iter()
                .enumerate()
                .map(|(index, _)| self.get_value_at_index(index))
                .collect(),
            Type::Array(_, array_len) => (0..*array_len)
                .into_iter()
                .map(|index| self.get_value_at_index(index))
                .collect(),
            Type::Pointer(inner, _) => match inner.as_ref() {
                Type::Slice(_) | Type::Str(_) => {
                    vec![self.get_value_at_index(0), self.get_value_at_index(1)]
                }
                Type::Array(_, size) => {
                    let mut values = vec![];

                    for i in 0..*size {
                        values.push(self.get_value_at_index(i));
                    }

                    values
                }
                ty => panic!("{:?}", ty),
            },
            ty => panic!("{:?}", ty),
        }
    }

    pub fn get_value_at_index(&self, index: usize) -> Value {
        let offset = self.ty.offset_of(index, WORD_SIZE);

        match &self.ty {
            Type::Unit => panic!("{}", index),
            Type::Struct(struct_type) => self.bytes.offset(offset).get_value(&struct_type.fields[index].ty),
            Type::Tuple(elements) => self.bytes.offset(offset).get_value(&elements[index]),
            Type::Array(ty, _) => self.bytes.offset(offset).get_value(ty),
            Type::Pointer(inner, _) => match inner.as_ref() {
                Type::Slice(ty) | Type::Str(ty) => match index {
                    0 => self.bytes.offset(offset).get_value(&Type::Pointer(ty.clone(), false)),
                    1 => self.bytes.offset(offset).get_value(&Type::uint()),
                    _ => panic!("{}", index),
                },
                _ => panic!("{:?}", &self.ty),
            },
            _ => panic!("{:?}", &self.ty),
        }
    }
}

#[derive(Debug, Clone)]
pub struct Function {
    pub id: hir::FunctionId,
    pub name: Ustr,
    pub ty: FunctionType,
    pub code: Bytecode,
}

#[derive(Debug, Clone)]
pub struct FunctionAddress {
    pub id: hir::FunctionId,
    pub is_extern: bool,
    pub name: Ustr,
}

#[derive(Debug, Clone)]
pub struct ExternFunction {
    pub lib_path: Ustr,
    pub name: Ustr,
    pub ty: FunctionType,
}

#[derive(Debug, Clone)]
pub struct ExternVariable {
    pub name: Ustr,
    pub lib: ExternLibrary,
    pub ty: Type,
}

#[derive(PartialEq, Debug, Clone, Copy)]
pub enum IntrinsicFunction {
    StartWorkspace,
}

impl From<hir::Intrinsic> for IntrinsicFunction {
    fn from(intrinsic: hir::Intrinsic) -> Self {
        match intrinsic {
            hir::Intrinsic::StartWorkspace => IntrinsicFunction::StartWorkspace,
            hir::Intrinsic::Location | hir::Intrinsic::CallerLocation => panic!(
                "intrinsic function '{}' should have been evaluated at compile-time",
                intrinsic
            ),
            hir::Intrinsic::Os | hir::Intrinsic::Arch => panic!("unexpected intrinsic variable '{}'", intrinsic),
        }
    }
}

#[derive(Debug, Clone)]
pub enum FunctionValue<'a> {
    Orphan(&'a Function),
    Extern(&'a ExternFunction),
}

impl Display for IntrinsicFunction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                IntrinsicFunction::StartWorkspace => "start_workspace",
            }
        )
    }
}

impl From<&Type> for ValueKind {
    fn from(ty: &Type) -> Self {
        match ty {
            Type::Never | Type::Unit => ValueKind::Buffer,
            Type::Bool => ValueKind::Bool,
            Type::Int(ty) => match ty {
                IntType::I8 => Self::I8,
                IntType::I16 => Self::I16,
                IntType::I32 => Self::I32,
                IntType::I64 => Self::I64,
                IntType::Int => Self::Int,
            },
            Type::Uint(ty) => match ty {
                UintType::U8 => Self::U8,
                UintType::U16 => Self::U16,
                UintType::U32 => Self::U32,
                UintType::U64 => Self::U64,
                UintType::Uint => Self::Uint,
            },
            Type::Float(ty) => match ty {
                FloatType::F16 | FloatType::F32 => Self::F32,
                FloatType::F64 => Self::F64,
                FloatType::Float => {
                    if IS_64BIT {
                        Self::F64
                    } else {
                        Self::F32
                    }
                }
            },
            Type::Pointer(inner, _) => match inner.as_ref() {
                Type::Slice(_) | Type::Str(_) => Self::Buffer,
                _ => Self::Pointer,
            },
            Type::Function(_) => Self::Function,
            Type::Array(_, _) | Type::Tuple(_) | Type::Struct(_) => Self::Buffer,
            Type::Module(_) => panic!(),
            Type::Type(_) => Self::Type,
            Type::Infer(_, InferType::AnyInt) => Self::Int,
            Type::Infer(_, InferType::AnyFloat) => {
                if IS_64BIT {
                    Self::F64
                } else {
                    Self::F32
                }
            }
            _ => panic!("invalid type {:?}", ty),
        }
    }
}

impl Value {
    pub fn unit() -> Self {
        Value::Buffer(Buffer {
            bytes: ByteSeq::new(0),
            ty: Type::Unit,
        })
    }

    pub unsafe fn from_type_and_ptr(ty: &Type, ptr: RawPointer) -> Self {
        match ty {
            Type::Never | Type::Unit => Self::unit(),
            Type::Bool => Self::Bool(*(ptr as *mut bool)),
            Type::Int(ty) => match ty {
                IntType::I8 => Self::I8(*(ptr as *mut i8)),
                IntType::I16 => Self::I16(*(ptr as *mut i16)),
                IntType::I32 => Self::I32(*(ptr as *mut i32)),
                IntType::I64 => Self::I64(*(ptr as *mut i64)),
                IntType::Int => Self::Int(*(ptr as *mut isize)),
            },
            Type::Uint(ty) => match ty {
                UintType::U8 => Self::U8(*(ptr as *mut u8)),
                UintType::U16 => Self::U16(*(ptr as *mut u16)),
                UintType::U32 => Self::U32(*(ptr as *mut u32)),
                UintType::U64 => Self::U64(*(ptr as *mut u64)),
                UintType::Uint => Self::Uint(*(ptr as *mut usize)),
            },
            Type::Float(ty) => match ty {
                FloatType::F16 | FloatType::F32 => Self::F32(*(ptr as *mut f32)),
                FloatType::F64 => Self::F64(*(ptr as *mut f64)),
                FloatType::Float => {
                    if IS_64BIT {
                        Self::F64(*(ptr as *mut f64))
                    } else {
                        Self::F32(*(ptr as *mut f32))
                    }
                }
            },
            Type::Pointer(ty, _) => Self::Pointer(Pointer::from_type_and_ptr(ty, *(ptr as *mut RawPointer))),
            Type::Function(_) => todo!(),
            Type::Array(inner, size) => Self::Buffer(Buffer {
                bytes: ByteSeq::copy_from_raw_parts(ptr as _, *size * inner.size_of(WORD_SIZE)),
                ty: ty.clone(),
            }),
            Type::Tuple(_) => {
                let size = ty.size_of(WORD_SIZE);
                let slice = slice::from_raw_parts(ptr as *const u8, size);

                Self::Buffer(Buffer {
                    bytes: ByteSeq::from(slice),
                    ty: ty.clone(),
                })
            }
            Type::Struct(struct_ty) => {
                let size = struct_ty.size_of(WORD_SIZE);
                let slice = slice::from_raw_parts(ptr as *const u8, size);

                Self::Buffer(Buffer {
                    bytes: ByteSeq::from(slice),
                    ty: ty.clone(),
                })
            }
            Type::Infer(_, InferType::AnyInt) => Self::Int(*(ptr as *mut isize)),
            Type::Infer(_, InferType::AnyFloat) => {
                if IS_64BIT {
                    Self::F64(*(ptr as *mut f64))
                } else {
                    Self::F32(*(ptr as *mut f32))
                }
            }
            _ => panic!("invalid type {:?}", ty),
        }
    }

    pub fn get_type(&self, interp: &Interp) -> Type {
        match self {
            Self::I8(_) => Type::i8(),
            Self::I16(_) => Type::i16(),
            Self::I32(_) => Type::i32(),
            Self::I64(_) => Type::i64(),
            Self::Int(_) => Type::int(),
            Self::U8(_) => Type::u8(),
            Self::U16(_) => Type::u16(),
            Self::U32(_) => Type::u32(),
            Self::U64(_) => Type::u64(),
            Self::Uint(_) => Type::uint(),
            Self::F32(_) => Type::f32(),
            Self::F64(_) => Type::f64(),
            Self::Bool(_) => Type::Bool,
            Self::Buffer(arr) => arr.ty.clone(),
            Self::Pointer(p) => Type::Pointer(Box::new(p.get_type()), true),
            Self::Function(f) => Type::Function(if f.is_extern {
                interp.extern_functions.get(&f.id).unwrap().ty.clone()
            } else {
                interp.functions.get(&f.id).unwrap().ty.clone()
            }),
            Self::ExternVariable(v) => v.ty.clone(),
            Self::Intrinsic(_) | Self::Type(_) => todo!(),
        }
    }

    pub fn try_into_const_value(
        self,
        tcx: &mut TypeCtx,
        ty: &Type,
        eval_span: Span,
    ) -> Result<ConstValue, &'static str> {
        match self {
            Self::I8(v) => Ok(ConstValue::Int(v as _)),
            Self::I16(v) => Ok(ConstValue::Int(v as _)),
            Self::I32(v) => Ok(ConstValue::Int(v as _)),
            Self::I64(v) => Ok(ConstValue::Int(v as _)),
            Self::Int(v) => Ok(ConstValue::Int(v as _)),
            Self::U8(v) => Ok(ConstValue::Int(v as _)),
            Self::U16(v) => Ok(ConstValue::Int(v as _)),
            Self::U32(v) => Ok(ConstValue::Int(v as _)),
            Self::U64(v) => Ok(ConstValue::Int(v as _)),
            Self::Uint(v) => Ok(ConstValue::Int(v as _)),
            Self::F32(v) => Ok(ConstValue::Float(v as _)),
            Self::F64(v) => Ok(ConstValue::Float(v)),
            Self::Bool(v) => Ok(ConstValue::Bool(v)),
            Self::Type(t) => Ok(ConstValue::Type(tcx.bound(t, eval_span))),
            Self::Buffer(buf) => match ty {
                Type::Unit => Ok(ConstValue::Unit(())),
                Type::Array(_, _) => {
                    let (el_ty, array_len) = if let Type::Array(el_ty, len) = buf.ty {
                        (el_ty, len)
                    } else {
                        panic!()
                    };

                    let mut values = Vec::with_capacity(array_len);
                    let el_size = el_ty.size_of(WORD_SIZE);

                    for i in 0..array_len {
                        let value = buf.bytes.offset(i * el_size).get_value(&el_ty);
                        let const_value = value.try_into_const_value(tcx, &el_ty, eval_span)?;
                        values.push(const_value);
                    }

                    Ok(ConstValue::Array(ConstArray {
                        values,
                        element_type: tcx.bound(*el_ty, eval_span),
                    }))
                }
                Type::Pointer(inner, _) => match inner.as_ref() {
                    Type::Slice(inner) | Type::Str(inner) => {
                        if matches!(inner.as_ref(), Type::Uint(UintType::U8)) {
                            let str = buf.as_str();
                            Ok(ConstValue::Str(ustr(str)))
                        } else {
                            Err("slice")
                        }
                    }
                    _ => panic!("value type mismatch. expected an aggregate type, got {:?}", ty),
                },
                Type::Tuple(elements) => {
                    let align = ty.align_of(WORD_SIZE);
                    let mut values = Vec::with_capacity(elements.len());

                    for (index, elem_type) in elements.iter().enumerate() {
                        let value = buf.bytes.offset(index * align).get_value(elem_type);
                        let const_value = value.try_into_const_value(tcx, ty, eval_span)?;
                        values.push(ConstElement {
                            value: const_value,
                            ty: tcx.bound(elem_type.clone(), eval_span),
                        });
                    }

                    Ok(ConstValue::Tuple(values))
                }
                Type::Struct(struct_type) => {
                    let align = ty.align_of(WORD_SIZE);
                    let mut fields = IndexMap::<Ustr, ConstElement>::new();

                    for (index, field) in struct_type.fields.iter().enumerate() {
                        let value = buf.bytes.offset(index * align).get_value(&field.ty);
                        let const_value = value.try_into_const_value(tcx, ty, eval_span)?;

                        fields.insert(
                            field.name,
                            ConstElement {
                                value: const_value,
                                ty: tcx.bound(field.ty.clone(), field.span),
                            },
                        );
                    }

                    Ok(ConstValue::Struct(fields))
                }
                ty => panic!("value type mismatch. expected an aggregate type, got {:?}", ty),
            },
            Self::Function(f) => Ok(ConstValue::Function(ConstFunction { id: f.id, name: f.name })),
            Self::ExternVariable(v) => Ok(ConstValue::ExternVariable(ConstExternVariable {
                name: v.name,
                lib: Some(v.lib.clone()),
                dylib: Some(v.lib),
                ty: tcx.bound(v.ty, eval_span),
            })),
            Self::Pointer(_) => Err("pointer"),
            Self::Intrinsic(_) => Err("intrinsic function"),
        }
    }
}

impl Pointer {
    #[allow(unused)]
    pub fn unit() -> Self {
        Pointer::Buffer(&mut Buffer {
            bytes: ByteSeq::new(0),
            ty: Type::Unit,
        })
    }

    pub fn from_type_and_ptr(ty: &Type, ptr: RawPointer) -> Self {
        match ty {
            Type::Never | Type::Unit => Self::U8(ptr as _),
            Type::Bool => Self::Bool(ptr as _),
            Type::Int(ty) => match ty {
                IntType::I8 => Self::I8(ptr as _),
                IntType::I16 => Self::I16(ptr as _),
                IntType::I32 => Self::I32(ptr as _),
                IntType::I64 => Self::I64(ptr as _),
                IntType::Int => Self::Int(ptr as _),
            },
            Type::Uint(ty) => match ty {
                UintType::U8 => Self::U8(ptr as _),
                UintType::U16 => Self::U16(ptr as _),
                UintType::U32 => Self::U32(ptr as _),
                UintType::U64 => Self::U64(ptr as _),
                UintType::Uint => Self::Uint(ptr as _),
            },
            Type::Float(ty) => match ty {
                FloatType::F16 | FloatType::F32 => Self::F32(ptr as _),
                FloatType::F64 => Self::F64(ptr as _),
                FloatType::Float => {
                    if IS_64BIT {
                        Self::F64(ptr as _)
                    } else {
                        Self::F32(ptr as _)
                    }
                }
            },
            Type::Pointer(inner, _) => match inner.as_ref() {
                Type::Slice(_) | Type::Str(_) => {
                    if ptr.is_null() {
                        Self::Buffer(std::ptr::null_mut())
                    } else {
                        let bytes = ByteSeq::copy_from_raw_parts(ptr as _, ty.size_of(WORD_SIZE));

                        let buf = Box::new(Buffer { bytes, ty: ty.clone() });

                        // Note (Ron): Leak
                        Self::Buffer(Box::leak(buf) as *mut Buffer)
                    }
                }
                _ => Self::from_type_and_ptr(inner, ptr),
            },
            Type::Function(_) => todo!(),
            Type::Array(inner, size) => {
                if ptr.is_null() {
                    Self::Buffer(std::ptr::null_mut())
                } else {
                    let bytes = ByteSeq::copy_from_raw_parts(ptr as _, *size * inner.size_of(WORD_SIZE));

                    let buf = Box::new(Buffer { bytes, ty: ty.clone() });

                    // Note (Ron): Leak
                    Self::Buffer(Box::leak(buf) as *mut Buffer)
                }
            }
            Type::Tuple(_) | Type::Struct(_) => {
                if ptr.is_null() {
                    Self::Buffer(std::ptr::null_mut())
                } else {
                    let bytes = ByteSeq::copy_from_raw_parts(ptr as _, ty.size_of(WORD_SIZE));

                    let buf = Box::new(Buffer { bytes, ty: ty.clone() });

                    // Note (Ron): Leak
                    Self::Buffer(Box::leak(buf) as *mut Buffer)
                }
            }
            Type::Infer(_, InferType::AnyInt) => Self::Int(ptr as _),
            Type::Infer(_, InferType::AnyFloat) => {
                if IS_64BIT {
                    Self::F32(ptr as _)
                } else {
                    Self::F64(ptr as _)
                }
            }
            _ => panic!("invalid type {:?}", ty),
        }
    }

    pub fn get_type(&self) -> Type {
        match self {
            Self::I8(_) => Type::i8(),
            Self::I16(_) => Type::i16(),
            Self::I32(_) => Type::i32(),
            Self::I64(_) => Type::i64(),
            Self::Int(_) => Type::int(),
            Self::U8(_) => Type::u8(),
            Self::U16(_) => Type::u16(),
            Self::U32(_) => Type::u32(),
            Self::U64(_) => Type::u64(),
            Self::Uint(_) => Type::uint(),
            Self::F32(_) => Type::f32(),
            Self::F64(_) => Type::f64(),
            Self::Bool(_) => Type::Bool,
            Self::Buffer(buf) => unsafe { &**buf }.ty.clone(),
            Self::Pointer(p) => Type::Pointer(
                if p.is_null() {
                    Box::new(Type::u8())
                } else {
                    Box::new(unsafe { &**p }.get_type())
                },
                true,
            ),
            Self::Function(_) => todo!(),
            value => panic!("{:?}", value),
        }
    }

    pub unsafe fn write_value(&self, value: Value) {
        unsafe fn slice<'a, T>(p: &*mut T) -> &'a mut [u8] {
            slice::from_raw_parts_mut(*p as *mut u8, mem::size_of::<T>())
        }

        match (self, value) {
            (Self::I8(p), Value::I8(v)) => slice(p).write_i8(v).unwrap(),
            (Self::I16(p), Value::I16(v)) => slice(p).write_i16::<NativeEndian>(v).unwrap(),
            (Self::I32(p), Value::I32(v)) => slice(p).write_i32::<NativeEndian>(v).unwrap(),
            (Self::I64(p), Value::I64(v)) => slice(p).write_i64::<NativeEndian>(v).unwrap(),
            (Self::Int(p), Value::Int(v)) => slice(p).write_int::<NativeEndian>(v as i64, WORD_SIZE).unwrap(),
            (Self::U8(p), Value::U8(v)) => slice(p).write_u8(v).unwrap(),
            (Self::U16(p), Value::U16(v)) => slice(p).write_u16::<NativeEndian>(v).unwrap(),
            (Self::U32(p), Value::U32(v)) => slice(p).write_u32::<NativeEndian>(v).unwrap(),
            (Self::U64(p), Value::U64(v)) => slice(p).write_u64::<NativeEndian>(v).unwrap(),
            (Self::Uint(p), Value::Uint(v)) => slice(p).write_uint::<NativeEndian>(v as u64, WORD_SIZE).unwrap(),
            (Self::F32(p), Value::F32(v)) => slice(p).write_f32::<NativeEndian>(v).unwrap(),
            (Self::F64(p), Value::F64(v)) => slice(p).write_f64::<NativeEndian>(v).unwrap(),
            (Self::Bool(p), Value::Bool(v)) => slice(p).write_u8(v as u8).unwrap(),
            (Self::Buffer(p), Value::Buffer(v)) => **p = v,
            (Self::Pointer(p), Value::Pointer(v)) => **p = v,
            (Self::Function(p), Value::Function(v)) => **p = v,
            (Self::Type(p), Value::Type(v)) => **p = v,
            (p, v) => panic!("invalid pair {:?} , {}", p, v.to_string()),
        }
    }
}

const MAX_CONSECUTIVE_VALUES: isize = 4;

impl Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Value::I8(v) => format!("i8 {}", v),
                Value::I16(v) => format!("i16 {}", v),
                Value::I32(v) => format!("i32 {}", v),
                Value::I64(v) => format!("i64 {}", v),
                Value::Int(v) => format!("int {}", v),
                Value::U8(v) => format!("u8 {}", v),
                Value::U16(v) => format!("u16 {}", v),
                Value::U32(v) => format!("u32 {}", v),
                Value::U64(v) => format!("u64 {}", v),
                Value::Uint(v) => format!("uint {}", v),
                Value::F32(v) => format!("f32 {:.2}", v),
                Value::F64(v) => format!("f64 {:.2}", v),
                Value::Bool(v) => format!("bool {}", v),
                Value::Buffer(v) => v.to_string(),
                Value::Pointer(p) => p.to_string(),
                Value::Function(f) => f.to_string(),
                Value::ExternVariable(v) => v.to_string(),
                Value::Intrinsic(v) => v.to_string(),
                Value::Type(ty) => format!("type {:?}", ty),
            }
        )
    }
}

impl Display for Buffer {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if self.ty.is_unit() || self.ty.is_never() {
            write!(f, "()")
        } else {
            let values_joined = self
                .get_all_values()
                .iter()
                .take(MAX_CONSECUTIVE_VALUES as _)
                .map(|v| v.to_string())
                .collect::<Vec<String>>()
                .join(", ");

            let len = match &self.ty {
                Type::Struct(s) => s.fields.len(),
                Type::Tuple(elements) => elements.len(),
                Type::Array(_, size) => *size,
                Type::Pointer(inner, _) => match inner.as_ref() {
                    Type::Slice(_) | Type::Str(_) => 2,
                    ty => panic!("{:?}", ty),
                },
                ty => panic!("{:?}", ty),
            };

            let extra_values = len as isize - MAX_CONSECUTIVE_VALUES;
            let extra_values_str = if extra_values > 0 {
                format!(", +{} more", extra_values)
            } else {
                "".to_string()
            };

            match &self.ty {
                Type::Struct(_) => {
                    write!(f, "{{{}{}}}", values_joined, extra_values_str)
                }
                Type::Tuple(_) => {
                    write!(f, "({}{})", values_joined, extra_values_str)
                }
                Type::Array(..) => {
                    write!(f, "[{}{}]", values_joined, extra_values_str)
                }
                Type::Pointer(inner, _) => match inner.as_ref() {
                    Type::Slice(_) | Type::Str(_) => write!(f, "&[{}{}]", values_joined, extra_values_str),
                    ty => panic!("{:?}", ty),
                },
                ty => panic!("{:?}", ty),
            }
        }
    }
}

impl Display for Function {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "fn {}", self.name)
    }
}

impl Display for FunctionAddress {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "fn {}", self.name)
    }
}

impl Display for ExternFunction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "extern fn {}", self.name)
    }
}

impl Display for ExternVariable {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "extern var {}", self.name)
    }
}

impl Display for Pointer {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let value = if self.as_inner_raw().is_null() {
            "null".to_string()
        } else {
            unsafe {
                match self {
                    Pointer::I8(v) => format!("i8 {}", **v),
                    Pointer::I16(v) => format!("i16 {}", **v),
                    Pointer::I32(v) => format!("i32 {}", **v),
                    Pointer::I64(v) => format!("i64 {}", **v),
                    Pointer::Int(v) => format!("int {}", **v),
                    Pointer::U8(v) => format!("u8 {}", **v),
                    Pointer::U16(v) => format!("u16 {}", **v),
                    Pointer::U32(v) => format!("u32 {}", **v),
                    Pointer::U64(v) => format!("u64 {}", **v),
                    Pointer::Uint(v) => format!("uint {}", **v),
                    Pointer::F32(v) => format!("f32 {:.2}", **v),
                    Pointer::F64(v) => format!("f64 {:.2}", **v),
                    Pointer::Bool(v) => format!("bool {}", **v),
                    Pointer::Buffer(v) => (**v).to_string(),
                    Pointer::Pointer(p) => (**p).to_string(),
                    Pointer::Function(f) => (**f).to_string(),
                    Pointer::ExternVariable(v) => (**v).to_string(),
                    Pointer::Intrinsic(v) => (**v).to_string(),
                    Pointer::Type(ty) => format!("type {:?}", (**ty)),
                }
            }
        };

        write!(f, "ptr {}", value)
    }
}
