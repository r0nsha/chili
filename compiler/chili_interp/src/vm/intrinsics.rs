use super::{instruction::Intrinsic, VM};

impl<'vm> VM<'vm> {
    pub(super) fn dispatch_intrinsic(&mut self, intrinsic: Intrinsic) {
        match intrinsic {
            Intrinsic::StartWorkspace => todo!("interp start_workspace"),
        }
    }
}
