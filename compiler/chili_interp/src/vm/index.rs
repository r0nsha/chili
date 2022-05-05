use crate::{
    value::{Pointer, Value},
    vm::VM,
};

impl<'vm> VM<'vm> {
    #[inline]
    pub(super) fn index(&mut self, value: Value, index: usize) {
        match value {
            Value::Pointer(ptr) => match ptr {
                Pointer::Aggregate(elements) => {
                    let elements = unsafe { &mut *elements };
                    let element = elements.get(index as usize).unwrap();
                    self.stack.push(element.clone())
                }
                ptr => {
                    // this is a pointer offset

                    let ptr = unsafe { &*ptr.into_pointer() };
                    let raw = ptr.as_inner_raw();

                    let offset = unsafe { raw.offset(index as isize) };
                    let offset_ptr = Pointer::from_kind_and_ptr(ptr.kind(), offset);

                    let value = unsafe { offset_ptr.deref_value() };

                    self.stack.push(value);
                }
            },
            Value::Aggregate(elements) => {
                self.stack
                    .push(elements.get(index as usize).unwrap().clone());
            }
            _ => panic!("invalid value {}", value.to_string()),
        }
    }

    #[inline]
    pub(super) fn index_ptr(&mut self, value: Value, index: usize) {
        match value {
            Value::Pointer(ptr) => match ptr {
                Pointer::Aggregate(elements) => {
                    let elements = unsafe { &mut *elements };
                    let element = elements.get_mut(index as usize).unwrap();
                    self.stack.push(Value::Pointer(element.into()))
                }
                ptr => {
                    // this is a pointer offset

                    // Note (Ron): I'm not sure if this first line is correct for all cases
                    let ptr = unsafe { &*ptr.into_pointer() };
                    let raw = ptr.as_inner_raw();
                    let offset = unsafe { raw.offset(index as isize) };

                    self.stack.push(Value::Pointer(Pointer::from_kind_and_ptr(
                        ptr.kind(),
                        offset,
                    )))
                }
            },
            Value::Aggregate(mut elements) => {
                let element = elements.get_mut(index as usize).unwrap();
                self.stack.push(Value::Pointer(element.into()))
            }
            _ => panic!("invalid value {}", value.to_string()),
        }
    }
}
