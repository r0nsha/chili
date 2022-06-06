use std::borrow::Cow;

use crate::cursor::Cursor;
use crate::source::Source;
use crate::unescape::{unescape, UnescapeError};
use crate::{
    Token,
    TokenKind::{self, *},
};
use chili_error::diagnostic::{Diagnostic, Label};
use chili_error::{DiagnosticResult, LexerError};
use unicode_xid::UnicodeXID;
use ustr::ustr;

pub(crate) const EOF_CHAR: char = '\0';
pub(crate) const EOF_STR: &str = "\0";
pub(crate) const DOUBLE_QUOTE: char = '"';

pub struct Lexer<'lx> {
    pub(crate) source: Source<'lx>,
    pub(crate) cursor: Cursor,
    pub(crate) tokens: Vec<Token>,
}

impl<'lx> Lexer<'lx> {
    pub fn new(file_id: usize, source: &'lx str) -> Self {
        Self {
            source: Source::new(source),
            cursor: Cursor::new(file_id),
            tokens: vec![],
        }
    }

    pub fn scan(mut self) -> DiagnosticResult<Vec<Token>> {
        self.eat_all_tokens()?;
        Ok(self.tokens)
    }

    pub(crate) fn eat_all_tokens(&mut self) -> DiagnosticResult<()> {
        while !self.is_eof() {
            let tt = self.eat_token()?;
            self.add_token(tt);
        }
        self.add_token(Eof);
        Ok(())
    }

    pub(crate) fn eat_token(&mut self) -> DiagnosticResult<TokenKind> {
        self.cursor.continue_from_end();

        let ch = self.bump();

        let tt = if is_id_start(ch) {
            if ch == 'r' && self.peek() == DOUBLE_QUOTE {
                self.eat_raw_str()?
            } else {
                self.eat_id()
            }
        } else {
            match ch {
                '@' => At,
                ';' => Semicolon,
                ':' => Colon,
                '(' => OpenParen,
                ')' => CloseParen,
                '[' => OpenBracket,
                ']' => CloseBracket,
                '{' => OpenCurly,
                '}' => CloseCurly,
                '+' => {
                    if self.is('=') {
                        PlusEq
                    } else {
                        Plus
                    }
                }
                '-' => {
                    if self.is('>') {
                        RightArrow
                    } else if self.is('=') {
                        MinusEq
                    } else {
                        Minus
                    }
                }
                '*' => {
                    if self.is('=') {
                        StarEq
                    } else {
                        Star
                    }
                }
                '?' => QuestionMark,
                '/' => {
                    if self.is('/') {
                        self.eat_comment();
                        self.eat_token()?
                    } else if self.is('*') {
                        self.eat_multiline_comment();
                        self.eat_token()?
                    } else if self.is('=') {
                        FwSlashEq
                    } else {
                        FwSlash
                    }
                }
                '.' => {
                    if self.is('.') {
                        DotDot
                    } else {
                        Dot
                    }
                }
                '%' => {
                    if self.is('=') {
                        PercentEq
                    } else {
                        Percent
                    }
                }
                ',' => Comma,
                // skip this character
                ' ' | '\r' | '\t' | '\n' => self.eat_token()?,
                '\'' => self.eat_char()?,
                DOUBLE_QUOTE => self.eat_str()?,
                '^' => {
                    if self.is('=') {
                        CaretEq
                    } else {
                        Caret
                    }
                }
                '&' => {
                    if self.is('&') {
                        if self.is('=') {
                            AmpAmpEq
                        } else {
                            AmpAmp
                        }
                    } else if self.is('=') {
                        AmpEq
                    } else {
                        Amp
                    }
                }
                '|' => {
                    if self.is('|') {
                        if self.is('=') {
                            BarBarEq
                        } else {
                            BarBar
                        }
                    } else if self.is('=') {
                        BarEq
                    } else {
                        Bar
                    }
                }
                '!' => {
                    if self.is('=') {
                        BangEq
                    } else {
                        Bang
                    }
                }
                '=' => {
                    if self.is('=') {
                        EqEq
                    } else {
                        Eq
                    }
                }
                '<' => {
                    if self.is('=') {
                        LtEq
                    } else if self.is('<') {
                        if self.is('=') {
                            LtLtEq
                        } else {
                            LtLt
                        }
                    } else {
                        Lt
                    }
                }
                '>' => {
                    if self.is('=') {
                        GtEq
                    } else if self.is('>') {
                        if self.is('=') {
                            GtGtEq
                        } else {
                            GtGt
                        }
                    } else {
                        Gt
                    }
                }
                EOF_CHAR => Eof,
                _ => {
                    if ch.is_ascii_digit() {
                        self.eat_number()?
                    } else {
                        let span = self.cursor.span();
                        let message = format!("invalid token `{}`", ch);
                        return Err(Diagnostic::error()
                            .with_message(message.clone())
                            .with_label(Label::primary(span, message)));
                    }
                }
            }
        };

        Ok(tt)
    }

    fn eat_char(&mut self) -> DiagnosticResult<TokenKind> {
        const WRAP_WITH: char = '\'';
        // TODO: fix unescaping issues
        // TODO: write my own unescape functionality
        while self.peek() != WRAP_WITH && !self.is_eof() {
            self.bump();
        }

        if self.is_eof() {
            let span = self.cursor.span();
            let message = "missing a terminating ' at the of string literal";
            return Err(Diagnostic::error()
                .with_message(message)
                .with_label(Label::primary(span, message)));
        }

        self.bump();

        let value = self.source.range(self.cursor);

        let slice = value
            .chars()
            .skip(1)
            .take_while(|x| *x != WRAP_WITH)
            .collect::<String>();

        match unescape(&slice, self.cursor.span()) {
            Ok(s) => {
                if s.len() != 1 {
                    let span = self.cursor.span();
                    let message = "character literal must be one character long";
                    return Err(Diagnostic::error()
                        .with_message(message)
                        .with_label(Label::primary(span, message)));
                }

                Ok(Char(s.chars().next().unwrap()))
            }
            Err(e) => match e {
                UnescapeError::InvalidEscapeSequence(span) => {
                    let message = "invalid escape sequence";
                    Err(Diagnostic::error()
                        .with_message(message)
                        .with_label(Label::primary(span, message)))
                }
            },
        }
    }

    fn eat_raw_str(&mut self) -> DiagnosticResult<TokenKind> {
        self.bump();

        while self.peek() != DOUBLE_QUOTE && !self.is_eof() {
            self.bump();
        }

        if self.is_eof() {
            let span = self.cursor.span();
            let message = "missing a terminating \" at the of string literal";
            return Err(Diagnostic::error()
                .with_message(message)
                .with_label(Label::primary(span, message)));
        }

        self.bump();

        let value = self.source.range(self.cursor);

        let slice = value
            .chars()
            .skip(2)
            .take_while(|x| *x != DOUBLE_QUOTE)
            .collect::<Cow<'_, str>>();

        Ok(Str(ustr(&slice)))
    }

    fn eat_str(&mut self) -> DiagnosticResult<TokenKind> {
        loop {
            if self.peek() == DOUBLE_QUOTE || self.is_eof() {
                break;
            } else if self.peek() == '\\' && self.peek_next() == '\"' {
                self.bump();
                self.bump();
            } else {
                self.bump();
            }
        }

        if self.is_eof() {
            let span = self.cursor.span();
            return Err(Diagnostic::error()
                .with_message(format!(
                    "missing a terminating {} at the of string literal",
                    DOUBLE_QUOTE
                ))
                .with_label(Label::primary(span, "missing terminator")));
        }

        self.bump();

        let value = self.source.range(self.cursor.range());

        let slice = value
            .chars()
            .skip(1)
            .take_while(|x| *x != DOUBLE_QUOTE)
            .collect::<String>();

        match unescape(&slice, self.cursor.span()) {
            Ok(s) => Ok(Str(ustr(&s))),
            Err(e) => match e {
                UnescapeError::InvalidEscapeSequence(span) => {
                    let message = "invalid escape sequence";
                    Err(Diagnostic::error()
                        .with_message(message)
                        .with_label(Label::primary(span, message)))
                }
            },
        }
    }

    fn eat_multiline_comment(&mut self) {
        self.bump();

        while self.peek_two() != "*/" && !self.is_eof() {
            if self.peek_two() == "/*" {
                self.eat_multiline_comment();
            }

            self.bump();
        }

        self.bump();
        self.bump();
    }

    fn eat_comment(&mut self) {
        while self.peek() != '\n' && !self.is_eof() {
            self.bump();
        }
    }

    fn eat_number(&mut self) -> DiagnosticResult<TokenKind> {
        if self.peek_previous() == '0' {
            match self.peek() {
                'x' | 'X' => return self.eat_number_hex(),
                'o' | 'O' => return self.eat_number_octal(),
                'b' | 'B' => return self.eat_number_binary(),
                _ => (),
            }
        }

        while self.peek().is_ascii_digit() || self.peek() == '_' {
            self.bump();
        }

        if self.peek() == '.' && self.peek_next().is_ascii_digit() {
            self.bump();

            while self.peek().is_ascii_digit() || self.peek() == '_' {
                self.bump();
            }

            let literal = self.source.range(self.cursor);
            let float = literal.replace('_', "").parse::<f64>().unwrap();

            Ok(Float(float))
        } else {
            let literal = self.source.range(self.cursor);
            match literal.replace('_', "").parse::<i64>() {
                Ok(i) => Ok(Int(i)),
                Err(_) => Err(LexerError::integer_too_large(self.cursor.span())),
            }
        }
    }

    fn eat_number_hex(&mut self) -> DiagnosticResult<TokenKind> {
        let mut hex_value = String::from("");

        self.bump();

        while !self.is_eof() {
            let char = self.peek();

            if ('0'..='9').contains(&char)
                || ('A'..='F').contains(&char)
                || ('a'..='f').contains(&char)
            {
                hex_value.push(char);
                self.bump();
            } else {
                break;
            }
        }

        let mut base = 1;
        let mut decimal_value: i64 = 0;

        for char in hex_value.chars().rev() {
            let to_add = if ('0'..='9').contains(&char) {
                Some((char as i64 - 48) * base)
            } else if ('A'..='F').contains(&char) {
                Some((char as i64 - 55) * base)
            } else if ('a'..='f').contains(&char) {
                Some((char as i64 - 87) * base)
            } else {
                None
            };

            if let Some(to_add) = to_add {
                decimal_value = match decimal_value.checked_add(to_add) {
                    Some(v) => v,
                    None => return Err(LexerError::integer_too_large(self.cursor.span())),
                };
                base *= 16;
            }
        }

        Ok(Int(decimal_value))
    }

    fn eat_number_octal(&mut self) -> DiagnosticResult<TokenKind> {
        let mut octal_value = String::from("");

        self.bump();

        while !self.is_eof() {
            let char = self.peek();

            if ('0'..='7').contains(&char) {
                octal_value.push(char);
                self.bump();
            } else {
                break;
            }
        }

        let mut base = 1;
        let mut decimal_value: i64 = 0;

        for char in octal_value.chars().rev() {
            let digit = char.to_digit(10).unwrap();
            decimal_value = match decimal_value.checked_add(digit as i64 * base) {
                Some(v) => v,
                None => return Err(LexerError::integer_too_large(self.cursor.span())),
            };
            base *= 8;
        }

        Ok(Int(decimal_value))
    }

    fn eat_number_binary(&mut self) -> DiagnosticResult<TokenKind> {
        let mut binary_value = String::from("");

        self.bump();

        while !self.is_eof() {
            let char = self.peek();

            if char == '0' || char == '1' {
                binary_value.push(char);
                self.bump();
            } else {
                break;
            }
        }

        let mut base = 1;
        let mut decimal_value: i64 = 0;

        for char in binary_value.chars().rev() {
            let digit = char.to_digit(10).unwrap();
            decimal_value = match decimal_value.checked_add(digit as i64 * base) {
                Some(v) => v,
                None => return Err(LexerError::integer_too_large(self.cursor.span())),
            };
            base *= 2;
        }

        Ok(Int(decimal_value))
    }

    #[inline]
    fn eat_id(&mut self) -> TokenKind {
        while is_id_continue(self.peek()) {
            self.bump();
        }

        match self.source.range(self.cursor) {
            "nil" => Nil,
            "true" => True,
            "false" => False,
            "if" => If,
            "else" => Else,
            "while" => While,
            "for" => For,
            "break" => Break,
            "continue" => Continue,
            "return" => Return,
            "defer" => Defer,
            "let" => Let,
            "fn" => Fn,
            "foreign" => Foreign,
            "pub" => Pub,
            "mut" => Mut,
            "in" => In,
            "as" => As,
            "struct" => Struct,
            "union" => Union,
            "match" => Match,
            "_" => Placeholder,
            l => Ident(ustr(l)),
        }
    }

    fn peek_two(&self) -> &str {
        if self.is_eof() {
            EOF_STR
        } else {
            self.source
                .range(self.cursor.end_index()..self.cursor.end_index() + 2)
        }
    }

    pub(crate) fn peek(&self) -> char {
        if self.is_eof() {
            EOF_CHAR
        } else {
            self.source.at(self.cursor.end_index())
        }
    }

    pub(crate) fn peek_previous(&self) -> char {
        self.source.at(self.cursor.end_index() - 1)
    }

    pub(crate) fn peek_next(&self) -> char {
        self.source.at(self.cursor.end_index() + 1)
    }

    pub(crate) fn is(&mut self, expected: char) -> bool {
        if self.peek() == expected {
            self.bump();
            true
        } else {
            false
        }
    }

    fn bump(&mut self) -> char {
        let char = self.peek();

        if !self.is_eof() {
            self.cursor.advance(char == '\n');
        }

        char
    }

    fn add_token(&mut self, kind: TokenKind) {
        self.tokens.push(Token {
            kind,
            lexeme: ustr(self.source.range(self.cursor)),
            span: self.cursor.span(),
        });
    }

    #[inline]
    fn is_eof(&self) -> bool {
        self.cursor.end_index() >= self.source.len()
    }
}

#[inline]
fn is_id_start(ch: char) -> bool {
    UnicodeXID::is_xid_start(ch) || ch == '_'
}

#[inline]
fn is_id_continue(ch: char) -> bool {
    UnicodeXID::is_xid_continue(ch) || ch == '_'
}
