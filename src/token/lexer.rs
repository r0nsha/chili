use super::cursor::Cursor;
use super::source::Source;
use super::unescape::{unescape, UnescapeError};
use super::{
    Token,
    TokenKind::{self, *},
};
use crate::error::diagnostic::{Diagnostic, Label};
use crate::error::{DiagnosticResult, LexerError, SyntaxError};
use unicode_xid::UnicodeXID;
use unindent::unindent;
use ustr::ustr;

pub const EOF_CHAR: char = '\0';
pub const DOUBLE_QUOTE: char = '"';
pub const SINGLE_QUOTE: char = '\'';

pub struct Lexer<'lx> {
    pub source: Source<'lx>,
    pub cursor: Cursor,
    pub tokens: Vec<Token>,
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

    pub fn eat_all_tokens(&mut self) -> DiagnosticResult<()> {
        while !self.is_eof() {
            let tt = self.eat_token()?;
            self.add_token(tt);
        }
        self.add_token(Eof);
        Ok(())
    }

    pub fn eat_token(&mut self) -> DiagnosticResult<TokenKind> {
        self.cursor.continue_from_end();

        let ch = self.bump();

        let tt = if is_id_start(ch) {
            self.eat_id()
        } else {
            match ch {
                '@' => At,
                '#' => {
                    // This is a comment, eat the rest of the line and then eat the next token.
                    self.eat_line();
                    self.eat_token()?
                }
                ';' => Semicolon,
                ':' => Colon,
                '(' => OpenParen,
                ')' => CloseParen,
                '[' => OpenBracket,
                ']' => CloseBracket,
                '{' => OpenCurly,
                '}' => CloseCurly,
                '+' => {
                    if self.eat('=') {
                        PlusEq
                    } else {
                        Plus
                    }
                }
                '-' => {
                    if self.eat('>') {
                        RightArrow
                    } else if self.eat('=') {
                        MinusEq
                    } else {
                        Minus
                    }
                }
                '*' => {
                    if self.eat('=') {
                        StarEq
                    } else {
                        Star
                    }
                }
                '?' => QuestionMark,
                '/' => {
                    if self.eat('=') {
                        FwSlashEq
                    } else {
                        FwSlash
                    }
                }
                '.' => {
                    if self.peek() == '.' && self.peek_next() == '.' {
                        self.bump();
                        self.bump();
                        DotDotDot
                    } else {
                        Dot
                    }
                }
                '%' => {
                    if self.eat('=') {
                        PercentEq
                    } else {
                        Percent
                    }
                }
                ',' => Comma,
                // skip this character
                ' ' | '\r' | '\t' => self.eat_token()?,
                '\n' => Newline,
                SINGLE_QUOTE => self.eat_char()?,
                DOUBLE_QUOTE => self.eat_str()?,
                '^' => {
                    if self.eat('=') {
                        CaretEq
                    } else {
                        Caret
                    }
                }
                '&' => {
                    if self.eat('&') {
                        if self.eat('=') {
                            AmpAmpEq
                        } else {
                            AmpAmp
                        }
                    } else if self.eat('=') {
                        AmpEq
                    } else {
                        Amp
                    }
                }
                '|' => {
                    if self.eat('|') {
                        if self.eat('=') {
                            BarBarEq
                        } else {
                            BarBar
                        }
                    } else if self.eat('=') {
                        BarEq
                    } else {
                        Bar
                    }
                }
                '!' => {
                    if self.eat('=') {
                        BangEq
                    } else {
                        Bang
                    }
                }
                '=' => {
                    if self.eat('=') {
                        EqEq
                    } else {
                        Eq
                    }
                }
                '<' => {
                    if self.eat('=') {
                        LtEq
                    } else if self.eat('<') {
                        if self.eat('=') {
                            LtLtEq
                        } else {
                            LtLt
                        }
                    } else {
                        Lt
                    }
                }
                '>' => {
                    if self.eat('=') {
                        GtEq
                    } else if self.eat('>') {
                        if self.eat('=') {
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
                        return Err(Diagnostic::error()
                            .with_message(format!("unknown character `{}`", ch))
                            .with_label(Label::primary(self.cursor.span(), "unknown character")));
                    }
                }
            }
        };

        Ok(tt)
    }

    fn eat_str(&mut self) -> DiagnosticResult<TokenKind> {
        while self.peek() != DOUBLE_QUOTE && !self.is_eof() {
            if self.peek() == '\\' && self.peek_next() == '"' {
                self.bump();
                self.bump();
            } else {
                self.bump();
            }
        }

        if self.is_eof() {
            return Err(Diagnostic::error()
                .with_message(format!(
                    "missing a terminating {} at the of string literal",
                    DOUBLE_QUOTE
                ))
                .with_label(Label::primary(self.cursor.span(), "missing terminator")));
        }

        self.expect(DOUBLE_QUOTE)?;

        let value = self.source.range(self.cursor.range());

        let mut chars = value.chars();

        chars.next();
        chars.next_back();

        let contents = chars.as_str().to_string();
        let contents = unindent(contents.trim());

        let contents = unescape(&contents, self.cursor.span()).map_err(|e| match e {
            UnescapeError::InvalidEscapeSequence(span) => {
                let message = "unknown escape sequence";
                Diagnostic::error()
                    .with_message(message)
                    .with_label(Label::primary(span, message))
            }
        })?;

        Ok(Str(ustr(&contents)))
    }

    fn eat_char(&mut self) -> DiagnosticResult<TokenKind> {
        while self.peek() != SINGLE_QUOTE && !self.is_eof() {
            if self.peek() == '\\' && self.peek_next() == '"' {
                self.bump();
                self.bump();
            } else {
                self.bump();
            }
        }

        if self.is_eof() {
            return Err(Diagnostic::error()
                .with_message(format!("missing a terminating {} at the of char literal", SINGLE_QUOTE))
                .with_label(Label::primary(self.cursor.span(), "missing terminator")));
        }

        self.expect(SINGLE_QUOTE)?;

        let value = self.source.range(self.cursor.range());

        let mut chars = value.chars();

        chars.next();
        chars.next_back();

        let contents = chars.as_str().to_string();

        if contents.len() != 1 {
            return Err(Diagnostic::error()
                .with_message("character literal must be one character long")
                .with_label(Label::primary(self.cursor.span(), "not one character long")));
        }

        let contents = unescape(&contents, self.cursor.span()).map_err(|e| match e {
            UnescapeError::InvalidEscapeSequence(span) => {
                let message = "unknown escape sequence";
                Diagnostic::error()
                    .with_message(message)
                    .with_label(Label::primary(span, message))
            }
        })?;

        Ok(Char(contents.chars().next().unwrap()))
    }

    fn eat_line(&mut self) {
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
            match literal.replace('_', "").parse::<u128>() {
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

            if ('0'..='9').contains(&char) || ('A'..='F').contains(&char) || ('a'..='f').contains(&char) {
                hex_value.push(char);
                self.bump();
            } else {
                break;
            }
        }

        match u128::from_str_radix(&hex_value, 16) {
            Ok(n) => Ok(Int(n)),
            Err(_) => Err(LexerError::integer_too_large(self.cursor.span())),
        }
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

        let mut base: u128 = 1;
        let mut decimal_value: u128 = 0;

        for char in octal_value.chars().rev() {
            let digit = char.to_digit(10).unwrap();
            decimal_value = match decimal_value.checked_add(digit as u128 * base) {
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

        let mut base: u128 = 1;
        let mut decimal_value: u128 = 0;

        for char in binary_value.chars().rev() {
            let digit = char.to_digit(10).unwrap();
            decimal_value = match decimal_value.checked_add(digit as u128 * base) {
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

        TokenKind::from(self.source.range(self.cursor))
    }

    pub fn peek(&self) -> char {
        if self.is_eof() {
            EOF_CHAR
        } else {
            self.source.at(self.cursor.end_index())
        }
    }

    pub fn peek_previous(&self) -> char {
        self.source.at(self.cursor.end_index() - 1)
    }

    pub fn peek_next(&self) -> char {
        self.source.at(self.cursor.end_index() + 1)
    }

    pub fn peek_offset(&self, offset: usize) -> char {
        self.source.at(self.cursor.end_index() + offset)
    }

    pub fn eat(&mut self, expected: char) -> bool {
        if self.peek() == expected {
            self.bump();
            true
        } else {
            false
        }
    }

    pub fn expect(&mut self, expected: char) -> DiagnosticResult<()> {
        self.eat(expected)
            .then(|| ())
            .ok_or_else(|| SyntaxError::expected(self.cursor.end_span(), &expected.to_string()))
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
