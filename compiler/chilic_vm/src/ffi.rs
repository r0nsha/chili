// use std::{ffi::c_void, os::raw::c_long, result::Result};

// use chilic_ty::*;
// use libffi::low::*;
// use ustr::Ustr;

// use crate::value::Value;

// #[derive(Debug, Clone)]
// pub struct ForeignFunction {
//     pub lib: Ustr,
//     pub name: Ustr,
//     pub param_tys: Vec<Ty>,
//     pub ret_ty: Ty,
//     pub variadic: bool,
// }

// pub struct FFI {}

// impl FFI {
//     pub fn new() -> Self {
//         Self {}
//     }

//     pub fn call(&mut self, func: ForeignFunction, values: Vec<Value>) ->
// Result<c_long, String> {         let lib_name = if func.lib.as_str() == "C" {
//             "msvcrt"
//         } else {
//             func.lib.as_str()
//         };

//         let lib = match unsafe {
// libloading::Library::new(format!("{}.dll", lib_name)) } {            
// Ok(lib) => lib,             Err(_) => return Err(format!("couldn't
// find library `{}`", lib_name)),         };

//         let symbol = match unsafe { lib.get(func.name.as_str().as_bytes()) }
// {             Ok(symbol) => symbol,
//             Err(_) => {
//                 return Err(format!(
//                     "couldn't find function `{}` in library `{}`",
//                     func.name, lib_name
//                 ))
//             }
//         };

//         let ret_ty: *mut ffi_type = unsafe { &mut ty_to_ffi_ty(&func.ret_ty)
// };

//         let mut param_tys: Vec<*mut ffi_type> = vec![];

//         unsafe {
//             for value in &values {
//                 param_tys.push(&mut value_to_ffi_ty(value));
//             }
//         };

//         let mut convention = ffi_cif::default();

//         unsafe {
//             let prep_result = if func.variadic {
//                 prep_cif_var(
//                     &mut convention,
//                     ffi_abi_FFI_DEFAULT_ABI,
//                     param_tys.len(),
//                     values.len(),
//                     ret_ty,
//                     param_tys.as_mut_ptr(),
//                 )
//             } else {
//                 prep_cif(
//                     &mut convention,
//                     ffi_abi_FFI_DEFAULT_ABI,
//                     param_tys.len(),
//                     ret_ty,
//                     param_tys.as_mut_ptr(),
//                 )
//             };

//             if let Err(_) = prep_result {
//                 return Err(format!("failed calling foreign function
// `{}`, this is probably a cause of mismatching parameter or return type
// definitions", func.name));             }
//         }

//         let mut args: Vec<*mut c_void> = vec![];

//         for value in values {
//             let arg = match value {
//                 Value::() => &mut 0 as *mut _ as *mut c_void,
//                 Value::Int(mut v) => &mut v as *mut _ as *mut c_void,
//                 Value::Bool(mut v) => &mut v as *mut _ as *mut c_void,
//                 Value::Str(mut v) => &mut v as *mut _ as *mut c_void,
//                 Value::Func(_) => panic!("bug! unexpected function to ffi
// conversion"),                 Value::ForeignFunc(_) => {
//                     panic!("bug! unexpected foreign function to ffi
// conversion")                 }
//             };

//             args.push(arg);
//         }

//         let result: c_long = unsafe {
//             call(
//                 &mut convention,
//                 CodePtr::from_ptr(*symbol),
//                 args.as_mut_ptr(),
//             )
//         };

//         Ok(result)
//     }
// }

// unsafe fn ty_to_ffi_ty(ty: &Ty) -> ffi_type {
//     match ty {
//         Ty::Bool => types::uint8,
//         Ty::I8 => types::sint8,
//         Ty::I16 => types::sint16,
//         Ty::I32 => types::sint32,
//         Ty::I64 => types::sint64,
//         Ty::() | Ty::Str | Ty::Func { .. } | Ty::Pointer(_) | Ty::Unknown =>
// types::pointer,     }
// }

// unsafe fn value_to_ffi_ty(value: &Value) -> ffi_type {
//     match value {
//         Value::Int(_) => types::sint64,
//         Value::Bool(_) => types::uint8,
//         Value::() | Value::Str(_) | Value::Func(_) | Value::ForeignFunc(_) =>
// types::pointer,     }
// }
