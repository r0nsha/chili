use super::{value::IntrinsicFunction, VM};

impl<'vm> VM<'vm> {
    pub(super) fn dispatch_intrinsic(&mut self, intrinsic: IntrinsicFunction) {
        match intrinsic {
            IntrinsicFunction::StartWorkspace => todo!("interp start_workspace"),
        }
    }
}
