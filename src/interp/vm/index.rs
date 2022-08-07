use super::{
    value::{Pointer, Value},
    VM,
};

impl<'vm> VM<'vm> {
    #[inline]
    pub fn index(&mut self, value: Value, index: usize) {
        match value {
            Value::Pointer(ref ptr) => match ptr {
                Pointer::Buffer(buf) => {
                    let buf = unsafe { &**buf };
                    let value = buf.get_value_at_index(index);
                    self.stack.push(value);
                }
                _ => panic!("invalid value {}", value.to_string()),
            },
            Value::Buffer(buf) => {
                let value = buf.get_value_at_index(index);
                self.stack.push(value);
            }
            _ => panic!("invalid value {}", value.to_string()),
        }
    }

    #[inline]
    pub fn index_ptr(&mut self, value: Value, index: usize) {
        match value {
            Value::Pointer(ref ptr) => match ptr {
                Pointer::Buffer(buf) => {
                    let buf = unsafe { &mut **buf };
                    let ptr = buf.bytes.offset_mut(index).as_mut_ptr();
                    let value = Value::Pointer(Pointer::from_type_and_ptr(buf.ty.element_type().unwrap(), ptr as _));
                    self.stack.push(value);
                }
                _ => panic!("invalid value {}", value.to_string()),
            },
            Value::Buffer(_) => self.offset(value, index),
            _ => panic!("invalid value {}", value.to_string()),
        }
    }

    #[inline]
    pub fn offset(&mut self, value: Value, offset: usize) {
        match value {
            Value::Pointer(ptr) => match ptr {
                Pointer::Buffer(buf) => {
                    let buf = unsafe { &mut *buf };
                    let ptr = buf.bytes.offset_mut(offset).as_mut_ptr();
                    let value = Value::Pointer(Pointer::from_type_and_ptr(&buf.ty, ptr as _));
                    self.stack.push(value);
                }
                ptr => {
                    let ptr = if ptr.is_pointer() {
                        unsafe { &*ptr.into_pointer() }
                    } else {
                        &ptr
                    };

                    let raw = ptr.as_inner_raw();
                    let offset = unsafe { raw.add(offset) };

                    self.stack
                        .push(Value::Pointer(Pointer::from_kind_and_ptr(ptr.kind(), offset)))
                }
            },
            Value::Buffer(buf) => {
                let bytes = buf.bytes.offset(offset);
                let ptr = &bytes[0];
                self.stack.push(Value::Pointer(Pointer::from_type_and_ptr(
                    &buf.ty,
                    ptr as *const u8 as *mut u8 as _,
                )));
            }
            _ => panic!("invalid value {}", value.to_string()),
        }
    }
}
