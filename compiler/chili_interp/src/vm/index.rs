use crate::{
    value::{bytes_get_value, Pointer, Value},
    vm::VM,
};

impl<'vm> VM<'vm> {
    #[inline]
    pub(super) fn index(&mut self, value: Value, index: usize) {
        match value {
            Value::Pointer(ptr) => match ptr {
                Pointer::Aggregate(elements) => {
                    let aggr = unsafe { &mut *elements };
                    let element = aggr.elements.get(index as usize).unwrap();
                    self.stack.push(element.clone())
                }
                Pointer::Array(array) => {
                    let array = unsafe { &mut *array };
                    let mut bytes = array.bytes.clone().split_off(index);
                    let value = bytes_get_value(&mut bytes, array.ty.inner());

                    self.stack.push(value.clone())
                }
                ptr => {
                    // this is a pointer offset

                    let ptr = if ptr.is_pointer() {
                        unsafe { &*ptr.into_pointer() }
                    } else {
                        &ptr
                    };

                    let raw = ptr.as_inner_raw();

                    let offset = unsafe { raw.offset(index as isize) };
                    let offset_ptr = Pointer::from_kind_and_ptr(ptr.kind(), offset);

                    let value = unsafe { offset_ptr.deref_value() };

                    self.stack.push(value);
                }
            },
            Value::Aggregate(aggr) => {
                self.stack
                    .push(aggr.elements.get(index as usize).unwrap().clone());
            }
            Value::Array(mut array) => {
                let mut bytes = array.bytes.split_off(index);
                let value = bytes_get_value(&mut bytes, array.ty.inner());
                self.stack.push(value)
            }
            _ => panic!("invalid value {}", value.to_string()),
        }
    }

    #[inline]
    pub(super) fn index_ptr(&mut self, value: Value, index: usize) {
        match value {
            Value::Pointer(ptr) => match ptr {
                Pointer::Aggregate(elements) => {
                    let aggr = unsafe { &mut *elements };
                    let element = aggr.elements.get_mut(index as usize).unwrap();
                    self.stack.push(Value::Pointer(element.into()))
                }
                Pointer::Array(array) => {
                    let array = unsafe { &mut *array };
                    let mut bytes = array.bytes.split_off(index);
                    let value = &mut bytes_get_value(&mut bytes, array.ty.inner());
                    array.bytes.unsplit(bytes);
                    self.stack.push(Value::Pointer(value.into()));
                }
                ptr => {
                    // this is a pointer offset

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
            Value::Aggregate(mut aggr) => {
                let element = aggr.elements.get_mut(index as usize).unwrap();
                self.stack.push(Value::Pointer(element.into()))
            }
            Value::Array(array) => {
                let mut bytes = array.bytes.clone().split_off(index);
                let value = &mut bytes_get_value(&mut bytes, array.ty.inner());
                self.stack.push(Value::Pointer(value.into()))
            }
            _ => panic!("invalid value {}", value.to_string()),
        }
    }
}
