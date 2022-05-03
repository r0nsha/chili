use crate::{
    instruction::CastInstruction,
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
                _ => panic!("invalid pointer {:?}", ptr),
            },
            Value::Aggregate(elements) => {
                self.stack
                    .push(elements.get(index as usize).unwrap().clone());
            }
            _ => panic!("invalid value {}", value),
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
                _ => panic!("invalid pointer {:?}", ptr),
            },
            Value::Aggregate(mut elements) => {
                let element = elements.get_mut(index as usize).unwrap();
                self.stack.push(Value::Pointer(element.into()))
            }
            _ => panic!("invalid value {}", value),
        }
    }
}
