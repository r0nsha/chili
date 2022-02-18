mod entity;
mod expr;
mod foreign;
mod func;
mod item;
mod literal;
mod pattern;
mod postfix_expr;
mod stmt;
mod ty;
mod r#use;

use bitflags::bitflags;
use chilic_error::{DiagnosticResult, SyntaxError};
use chilic_ir::{item::Items, module::ModuleInfo};
use chilic_span::Span;
use chilic_token::{
    Token,
    TokenType::{self, *},
};
use ustr::{ustr, Ustr};

bitflags! {
    struct Restrictions : u8 {
        const NO_STRUCT_LITERAL = 1 << 0;
    }
}

pub struct Parser {
    tokens: Vec<Token>,
    current: usize,
    marked: Vec<usize>,
    module_info: ModuleInfo,
    root_dir: String,
    current_dir: String,
    decl_name_frames: Vec<Ustr>,
    used_modules: Vec<ModuleInfo>,
    restrictions: Restrictions,
}

pub struct ParserResult {
    pub items: Items,
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
            marked: vec![],
            module_info,
            root_dir,
            current_dir,
            decl_name_frames: vec![],
            used_modules: vec![],
            restrictions: Restrictions::empty(),
        }
    }

    pub fn parse(&mut self) -> DiagnosticResult<ParserResult> {
        let mut items: Items = Items::new();

        while !self.is_end() {
            match self.parse_item() {
                Ok(i) => items.extend(i),
                Err(why) => {
                    return Err(why);
                }
            };

            self.skip_redundant_tokens();
        }

        Ok(ParserResult {
            items,
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
        while self.check_token(Semicolon) {
            self.bump();
        }
    }

    pub(crate) fn consume_str(&mut self) -> DiagnosticResult<Ustr> {
        let token = self.consume(Str(ustr("")))?;
        match token.token_type {
            Str(str) => Ok(str),
            _ => unreachable!(),
        }
    }

    pub(crate) fn consume_id(&mut self) -> DiagnosticResult<Token> {
        self.consume(Id(ustr("")))
    }

    pub(crate) fn consume(
        &mut self,
        token_type: TokenType,
    ) -> DiagnosticResult<Token> {
        if self.check_token(token_type.clone()) {
            Ok(self.bump().clone())
        } else {
            Err(SyntaxError::expected(self.span_ref(), token_type.lexeme()))
        }
    }

    pub(crate) fn consume_line_terminator(
        &mut self,
    ) -> DiagnosticResult<Token> {
        if self.match_line_terminator() {
            Ok(self.previous().clone())
        } else {
            Err(SyntaxError::expected(
                &self.previous_span_ref().end(),
                Semicolon.lexeme(),
            ))
        }
    }

    pub(crate) fn revert(&mut self, tokens: usize) {
        self.current -= tokens;
    }

    pub(crate) fn match_id(&mut self) -> bool {
        self.match_one(Id(ustr("")))
    }

    pub(crate) fn match_line_terminator(&mut self) -> bool {
        self.match_one(Semicolon)
    }

    pub(crate) fn match_one(&mut self, token_type: TokenType) -> bool {
        if self.check_token(token_type.clone()) {
            self.bump();
            true
        } else {
            false
        }
    }

    pub(crate) fn match_any(&mut self, token_types: &[TokenType]) -> bool {
        for token_type in token_types {
            if self.match_one(token_type.clone()) {
                return true;
            }
        }

        false
    }

    pub(crate) fn check_token(&mut self, token_type: TokenType) -> bool {
        if self.is_end() {
            false
        } else {
            // * if the current token doesn't carry any semantic meaning (i.e: a
            //   comment), then skip it.
            if !matches!(token_type, Comment(_))
                && matches!(self.peek().token_type, Comment(_))
            {
                self.bump();
                self.check_token(token_type)
            } else {
                self.peek().is(token_type)
            }
        }
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
