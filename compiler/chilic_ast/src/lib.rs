use entity::Entity;
use module::ModuleInfo;
use r#use::Use;

pub mod entity;
pub mod expr;
pub mod foreign_library;
pub mod func;
pub mod ir;
pub mod module;
pub mod op;
pub mod path;
pub mod pattern;
pub mod pretty;
pub mod r#use;
pub mod value;

pub const PLACEHOLDER_SYMBOL: &str = "_";

pub struct Ast {
    pub module_info: ModuleInfo,
    pub uses: Vec<Use>,
    pub entities: Vec<Entity>,
}

impl Ast {
    pub fn new(module_info: ModuleInfo) -> Self {
        Self {
            module_info,
            uses: vec![],
            entities: vec![],
        }
    }
}
