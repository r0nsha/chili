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

use std::collections::HashSet;

use bitflags::bitflags;
use chilic_ast::{foreign_library::ForeignLibrary, module::ModuleInfo, Ast};
use chilic_error::{DiagnosticResult, SyntaxError};
use chilic_span::Span;
use chilic_token::{Token, TokenType::*};
use ustr::{ustr, Ustr};

bitflags! {
    struct Restrictions : u8 {
        const NO_STRUCT_LITERAL = 1 << 0;
    }
}

macro_rules! last_is {
    ($parser:expr, $(|) ? $($pattern : pat_param) | +) => {
        match &$parser.previous().token_type {
            $( $pattern )|+ => true,
            _ => false
        }
    };
}
macro_rules! is {
    ($parser:expr, $(|) ? $($pattern : pat_param) | +) => {
        if $parser.is_end() {
            false
        } else {
            match &$parser.peek().token_type {
                $( $pattern )|+ => true,
                _ => false
            }
        }
    };
}

macro_rules! match_token {
    ($parser:expr, $(|) ? $($pattern : pat_param) | +) => {
        if is!($parser, $( $pattern )|+) {
            $parser.bump();
            true
        } else {
            false
        }
    };
}

macro_rules! require {
    ($parser:expr, $(|) ? $($pattern : pat_param) | +, $msg:literal) => {
        if is!($parser, $( $pattern )|+) {
            Ok($parser.bump().clone())
        } else {
            Err(SyntaxError::expected($parser.span_ref(), $msg))
        }
    };
}

macro_rules! parse_delimited_list {
    ($parser:expr, $close_delim:pat, $sep:pat, $parse:expr, $msg:literal) => {{
        let mut items = vec![];

        while !match_token!($parser, $close_delim) && !$parser.is_end() {
            let i = $parse;

            items.push(i);

            if match_token!($parser, $sep) {
                continue;
            } else if match_token!($parser, $close_delim) {
                break;
            } else {
                return Err(SyntaxError::expected($parser.span_ref(), $msg));
            }
        }

        items
    }};
}

pub(crate) use is;
pub(crate) use last_is;
pub(crate) use match_token;
pub(crate) use parse_delimited_list;
pub(crate) use require;

pub struct Parser {
    tokens: Vec<Token>,
    current: usize,
    marked: Vec<usize>,
    module_info: ModuleInfo,
    root_dir: String,
    current_dir: String,
    decl_name_frames: Vec<Ustr>,
    used_modules: Vec<ModuleInfo>,
    foreign_libraries: HashSet<ForeignLibrary>,
    restrictions: Restrictions,
}

pub struct ParserResult {
    pub ast: Ast,
    pub uses: Vec<ModuleInfo>,
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

    pub(crate) fn is_res(&self, restrictions: Restrictions) -> bool {
        self.restrictions.contains(restrictions)
    }

    pub(crate) fn get_decl_name(&self) -> Ustr {
        if !self.decl_name_frames.is_empty() {
            *self.decl_name_frames.last().unwrap()
        } else {
            ustr("")
        }
    }

    pub(crate) fn skip_redundant_tokens(&mut self) {
        while is!(self, Semicolon) {
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
        self.peek().token_type == Eof
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
        self.peek().span.clone()
    }

    pub(crate) fn span_ref(&self) -> &Span {
        &self.peek().span
    }

    #[allow(unused)]
    pub(crate) fn previous_span(&self) -> Span {
        self.previous().span.clone()
    }

    pub(crate) fn previous_span_ref(&self) -> &Span {
        &self.previous().span
    }
}
