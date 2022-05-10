use crate::{
    byte_seq::GetValue,
    value::{Pointer, Value},
    vm::VM,
};

impl<'vm> VM<'vm> {
    #[inline]
    pub(super) fn index(&mut self, value: Value, index: usize) {
        match value {
            Value::Pointer(ref ptr) => match ptr {
                Pointer::Aggregate(elements) => {
                    let aggr = unsafe { &**elements };
                    let element = &aggr.elements[index as usize];
                    self.stack.push(element.clone())
                }
                Pointer::Array(array) => {
                    let array = unsafe { &**array };
                    let value = array.bytes.offset(index).get_value(array.ty.inner());
                    self.stack.push(value);
                }
                _ => {
                    self.offset(value, index);
                    let ptr = self.stack.pop().into_pointer();
                    self.stack.push(unsafe { ptr.deref_value() });
                }
            },
            Value::Aggregate(aggr) => {
                self.stack.push(aggr.elements[index as usize].clone());
            }
            Value::Array(array) => {
                let value = array.bytes.offset(index).get_value(array.ty.inner());
                self.stack.push(value);
            }
            _ => panic!("invalid value {}", value.to_string()),
        }
    }

    #[inline]
    pub(super) fn index_ptr(&mut self, value: Value, index: usize) {
        match value {
            Value::Pointer(ref ptr) => match ptr {
                Pointer::Aggregate(elements) => {
                    let aggr = unsafe { &mut **elements };
                    let element = &mut aggr.elements[index as usize];
                    self.stack.push(Value::Pointer(element.into()))
                }
                Pointer::Array(array) => {
                    let array = unsafe { &mut **array };
                    let ptr = array.bytes.offset_mut(index).as_mut_ptr();
                    let value =
                        Value::Pointer(Pointer::from_type_and_ptr(array.ty.inner(), ptr as _));
                    self.stack.push(value);
                }
                _ => self.offset(value, index),
            },
            Value::Aggregate(mut aggr) => {
                let element = &mut aggr.elements[index as usize];
                self.stack.push(Value::Pointer(element.into()))
            }
            Value::Array(_) => self.offset(value, index),
            _ => panic!("invalid value {}", value.to_string()),
        }
    }

    #[inline]
    pub(super) fn offset(&mut self, value: Value, index: usize) {
        match value {
            Value::Pointer(ptr) => match ptr {
                Pointer::Array(array) => {
                    let array = unsafe { &mut *array };
                    let ptr = array.bytes.offset_mut(index).as_mut_ptr();
                    let value = Value::Pointer(Pointer::from_type_and_ptr(&array.ty, ptr as _));
                    self.stack.push(value);
                }
                ptr => {
                    let ptr = if ptr.is_pointer() {
                        unsafe { &*ptr.into_pointer() }
                    } else {
                        &ptr
                    };

                    let raw = ptr.as_inner_raw();
                    let offset = unsafe { raw.offset(index as isize) };

                    self.stack.push(Value::Pointer(Pointer::from_kind_and_ptr(
                        ptr.kind(),
                        offset,
                    )))
                }
            },
            Value::Array(array) => {
                let bytes = array.bytes.offset(index);
                let ptr = &bytes[0];
                self.stack.push(Value::Pointer(Pointer::from_type_and_ptr(
                    &array.ty,
                    ptr as *const u8 as *mut u8 as _,
                )));
            }
            _ => panic!("invalid value {}", value.to_string()),
        }
    }
}
