mod entity;
mod expr;
mod foreign;
mod func;
mod literal;
mod pattern;
mod postfix_expr;
mod top_level;
mod ty;
mod r#use;

use bitflags::bitflags;
use chilic_ast::ast::{Ast, ForeignLibrary, ModuleInfo};
use chilic_error::{DiagnosticResult, SyntaxError};
use chilic_span::Span;
use chilic_token::{Token, TokenKind::*};
use std::collections::HashSet;
use ustr::{ustr, Ustr};

bitflags! {
    struct Restrictions : u8 {
        const STMT_EXPR = 1 << 0;
        const NO_STRUCT_LITERAL = 1 << 1;
    }
}

macro_rules! last_token_is {
    ($parser:expr, $(|) ? $($pattern : pat_param) | +) => {
        match &$parser.previous().kind {
            $( $pattern )|+ => true,
            _ => false
        }
    };
}
macro_rules! token_is {
    ($parser:expr, $(|) ? $($pattern : pat_param) | +) => {
        if $parser.is_end() {
            false
        } else {
            match &$parser.peek().kind {
                $( $pattern )|+ => true,
                _ => false
            }
        }
    };
}

macro_rules! eat {
    ($parser:expr, $(|) ? $($pattern : pat_param) | +) => {
        if token_is!($parser, $( $pattern )|+) {
            $parser.bump();
            true
        } else {
            false
        }
    };
}

macro_rules! expect {
    ($parser:expr, $(|) ? $($pattern : pat_param) | +, $msg:literal) => {
        if token_is!($parser, $( $pattern )|+) {
            Ok($parser.bump().clone())
        } else {
            Err(SyntaxError::expected($parser.span(), $msg))
        }
    };
}

macro_rules! parse_delimited_list {
    ($parser:expr, $close_delim:pat, $sep:pat, $parse:expr, $msg:literal) => {{
        let mut items = vec![];

        while !eat!($parser, $close_delim) && !$parser.is_end() {
            let i = $parse;

            items.push(i);

            if eat!($parser, $sep) {
                continue;
            } else if eat!($parser, $close_delim) {
                break;
            } else {
                return Err(SyntaxError::expected($parser.span(), $msg));
            }
        }

        items
    }};
}

pub(crate) use eat;
pub(crate) use expect;
pub(crate) use last_token_is;
pub(crate) use parse_delimited_list;
pub(crate) use token_is;

pub struct Parser {
    tokens: Vec<Token>,
    current: usize,
    marked: Vec<usize>,
    module_info: ModuleInfo,
    root_dir: String,
    current_dir: String,
    decl_name_frames: Vec<Ustr>,
    used_modules: HashSet<ModuleInfo>,
    foreign_libraries: HashSet<ForeignLibrary>,
    restrictions: Restrictions,
}

pub struct ParserResult {
    pub ast: Ast,
    pub uses: HashSet<ModuleInfo>,
}

impl Parser {
    pub fn new(
        tokens: Vec<Token>,
        module_info: ModuleInfo,
        root_dir: String,
        current_dir: String,
    ) -> Self {
        Self {
            tokens,
            current: 0,
            marked: Default::default(),
            module_info,
            root_dir,
            current_dir,
            decl_name_frames: Default::default(),
            used_modules: Default::default(),
            foreign_libraries: Default::default(),
            restrictions: Restrictions::empty(),
        }
    }

    pub fn parse(&mut self) -> DiagnosticResult<ParserResult> {
        let mut ast = Ast::new(self.module_info);

        while !self.is_end() {
            self.parse_top_level(&mut ast)?;
            self.skip_redundant_tokens();
        }

        Ok(ParserResult {
            ast,
            uses: self.used_modules.clone(),
        })
    }

    pub(crate) fn with_res<T>(
        &mut self,
        restrictions: Restrictions,
        f: impl FnOnce(&mut Self) -> T,
    ) -> T {
        let old = self.restrictions;
        self.restrictions = restrictions;
        let res = f(self);
        self.restrictions = old;
        res
    }

    pub(crate) fn get_decl_name(&self) -> Ustr {
        if !self.decl_name_frames.is_empty() {
            *self.decl_name_frames.last().unwrap()
        } else {
            ustr("")
        }
    }

    pub(crate) fn skip_redundant_tokens(&mut self) {
        while token_is!(self, Semicolon) {
            self.bump();
        }
    }

    pub(crate) fn revert(&mut self, tokens: usize) {
        self.current -= tokens;
    }

    pub(crate) fn bump(&mut self) -> &Token {
        if !self.is_end() {
            self.current += 1;
        }

        self.previous()
    }

    pub(crate) fn is_end(&self) -> bool {
        self.peek().kind == Eof
    }

    pub(crate) fn mark(&mut self, offset: isize) {
        self.marked.push((self.current as isize + offset) as usize);
    }

    pub(crate) fn reset_to_mark(&mut self) {
        self.current = self.marked.pop().unwrap();
    }

    pub(crate) fn pop_mark(&mut self) {
        self.marked.pop();
    }

    pub(crate) fn peek(&self) -> &Token {
        self.tokens.get(self.current).unwrap()
    }

    pub(crate) fn previous(&self) -> &Token {
        self.tokens.get(self.current - 1).unwrap()
    }

    pub(crate) fn span(&self) -> Span {
        self.peek().span
    }

    pub(crate) fn previous_span(&self) -> Span {
        self.previous().span
    }
}
