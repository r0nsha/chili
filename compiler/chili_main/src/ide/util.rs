use chili_ast::workspace::{BindingInfo, Workspace};
use chili_span::Span;

#[inline]
pub fn find_offset_in_root_module(workspace: &Workspace, offset: usize) -> Option<&BindingInfo> {
    workspace.binding_infos.iter().find(|binding_info| {
        binding_info.module_id == workspace.root_module_id && binding_info.span.contains(offset)
    })
}

#[inline]
pub fn is_offset_in_span_and_root_module(workspace: &Workspace, offset: usize, span: Span) -> bool {
    span.contains(offset)
        && workspace
            .find_module_id_by_file_id(span.file_id)
            .map_or(false, |module_id| module_id == workspace.root_module_id)
}

#[inline]
pub fn write<T>(value: &T)
where
    T: ?Sized + serde::Serialize,
{
    println!("{}", serde_json::to_string(value).unwrap())
}

#[inline]
pub fn write_null() {
    println!("null")
}
