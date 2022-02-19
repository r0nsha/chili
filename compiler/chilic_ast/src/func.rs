use chilic_ty::Ty;
use ustr::Ustr;

use crate::{
    expr::{Block, Expr},
    pattern::Pattern,
};

#[derive(Debug, PartialEq, Clone)]
pub struct Fn {
    pub proto: Proto,
    pub body: Block,
    pub is_startup: bool,
}

#[derive(Debug, PartialEq, Clone)]
pub struct Proto {
    pub name: Ustr,
    pub params: Vec<FnParam>,
    pub variadic: bool,
    pub ret: Option<Box<Expr>>,
    pub lib_name: Option<Ustr>,
    pub ty: Ty,
}

#[derive(Debug, PartialEq, Clone)]
pub struct FnParam {
    pub pattern: Pattern,
    pub ty: Option<Box<Expr>>,
}

impl ToString for FnParam {
    fn to_string(&self) -> String {
        self.pattern.to_string()
    }
}
