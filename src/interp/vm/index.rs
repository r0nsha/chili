use super::{
    byte_seq::GetValue,
    value::{Pointer, Value},
    VM,
};

impl<'vm> VM<'vm> {
    #[inline]
    pub fn index(&mut self, value: Value, index: usize) {
        match value {
            Value::Pointer(ref ptr) => match ptr {
                Pointer::Buffer(array) => {
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
            Value::Buffer(array) => {
                let value = array.bytes.offset(index).get_value(array.ty.inner());
                self.stack.push(value);
            }
            _ => panic!("invalid value {}", value.to_string()),
        }
    }

    #[inline]
    pub fn index_ptr(&mut self, value: Value, index: usize) {
        match value {
            Value::Pointer(ref ptr) => match ptr {
                Pointer::Buffer(array) => {
                    let array = unsafe { &mut **array };
                    let ptr = array.bytes.offset_mut(index).as_mut_ptr();
                    let value =
                        Value::Pointer(Pointer::from_type_and_ptr(array.ty.inner(), ptr as _));
                    self.stack.push(value);
                }
                _ => self.offset(value, index),
            },
            Value::Buffer(_) => self.offset(value, index),
            _ => panic!("invalid value {}", value.to_string()),
        }
    }

    #[inline]
    pub fn offset(&mut self, value: Value, index: usize) {
        match value {
            Value::Pointer(ptr) => match ptr {
                Pointer::Buffer(array) => {
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
                    let offset = unsafe { raw.add(index) };

                    self.stack.push(Value::Pointer(Pointer::from_kind_and_ptr(
                        ptr.kind(),
                        offset,
                    )))
                }
            },
            Value::Buffer(array) => {
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
