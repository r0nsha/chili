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
pub const RAW_STR_PREFIX: char = 'r';
pub const CHAR_PREFIX: char = 'c';

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
            if ch == RAW_STR_PREFIX && self.peek() == DOUBLE_QUOTE {
                self.eat_raw_str()?
            } else if ch == CHAR_PREFIX && self.peek() == DOUBLE_QUOTE {
                self.eat_char()?
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
                    if self.eat('/') {
                        self.eat_comment();
                        self.eat_token()?
                    } else if self.eat('=') {
                        FwSlashEq
                    } else {
                        FwSlash
                    }
                }
                '.' => {
                    if self.eat('.') {
                        if self.eat('.') {
                            DotDotDot
                        } else {
                            DotDot
                        }
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
                ' ' | '\r' | '\t' | '\n' => self.eat_token()?,
                DOUBLE_QUOTE => {
                    if self.eat(DOUBLE_QUOTE) {
                        if self.eat(DOUBLE_QUOTE) {
                            // this is a multiline string
                            self.eat_str(StrFlavor::Multiline)?
                        } else {
                            // this is just an empty string, we can return an empty Str
                            Str(ustr(""))
                        }
                    } else {
                        self.eat_str(StrFlavor::Normal)?
                    }
                }
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

    fn eat_char(&mut self) -> DiagnosticResult<TokenKind> {
        self.bump();
        self.eat_str(StrFlavor::Char)
    }

    fn eat_raw_str(&mut self) -> DiagnosticResult<TokenKind> {
        self.bump();
        self.eat_str(StrFlavor::Raw)
    }

    fn eat_str(&mut self, flavor: StrFlavor) -> DiagnosticResult<TokenKind> {
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

        if flavor == StrFlavor::Multiline {
            self.expect(DOUBLE_QUOTE)?;
            self.expect(DOUBLE_QUOTE)?;
            self.expect(DOUBLE_QUOTE)?;
        } else {
            self.expect(DOUBLE_QUOTE)?;
        }

        let value = match flavor {
            StrFlavor::Normal | StrFlavor::Multiline => self.source.range(self.cursor.range()),
            StrFlavor::Raw | StrFlavor::Char => self
                .source
                .range(self.cursor.start_index() + 1..self.cursor.end_index()),
        };

        let mut chars = value.chars();

        if flavor == StrFlavor::Multiline {
            chars.next();
            chars.next();
            chars.next();
            chars.next_back();
            chars.next_back();
            chars.next_back();
        } else {
            chars.next();
            chars.next_back();
        }

        let contents = chars.as_str().to_string();

        match flavor {
            StrFlavor::Raw => Ok(Str(ustr(&contents))),
            flavor => {
                match flavor {
                    StrFlavor::Normal if contents.contains('\n') => {
                        return Err(Diagnostic::error()
                            .with_message("string cannot contain newline characters")
                            .with_label(Label::primary(
                                self.cursor.span(),
                                "cannot contain newline",
                            ))
                            .with_note(
                                "use a multiline string instead, example: \"\"\"your string\"\"\"",
                            ))
                    }
                    StrFlavor::Char if contents.len() != 1 => {
                        return Err(Diagnostic::error()
                            .with_message("character literal must be one character long")
                            .with_label(Label::primary(
                                self.cursor.span(),
                                "not one character long",
                            )))
                    }
                    _ => (),
                }

                let contents = if matches!(flavor, StrFlavor::Multiline) {
                    unindent(contents.trim())
                } else {
                    contents
                };

                let contents = unescape(&contents, self.cursor.span()).map_err(|e| match e {
                    UnescapeError::InvalidEscapeSequence(span) => {
                        let message = "unknown escape sequence";
                        Diagnostic::error()
                            .with_message(message)
                            .with_label(Label::primary(span, message))
                    }
                })?;

                if flavor == StrFlavor::Char {
                    Ok(Char(contents.chars().next().unwrap()))
                } else {
                    Ok(Str(ustr(&contents)))
                }
            }
        }
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

#[derive(Debug, PartialEq, Eq)]
enum StrFlavor {
    Normal,    // single line, with escape sequences
    Raw,       // multiline, no escape sequences
    Multiline, // multiline, with escape sequences
    Char,      // single character, with escape sequences
}
