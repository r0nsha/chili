mod cursor;
pub mod lexer;
mod source;
mod unescape;

use crate::span::Span;
use std::fmt::Display;
use ustr::{ustr, Ustr};

#[derive(Debug, Clone, Copy)]
pub struct Token {
    pub kind: TokenKind,
    pub lexeme: Ustr,
    pub span: Span,
}

impl Token {
    pub fn name(&self) -> Ustr {
        match &self.kind {
            TokenKind::Ident(name) => *name,
            TokenKind::Str(value) => ustr(value),
            _ => panic!("BUG! only call get_name for identifiers and strings"),
        }
    }
}

impl Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.kind)
    }
}

#[derive(strum_macros::Display, Debug, PartialEq, Clone, Copy)]
pub enum TokenKind {
    // Delimiters
    OpenParen,
    CloseParen,
    OpenCurly,
    CloseCurly,
    OpenBracket,
    CloseBracket,
    Semicolon,
    Colon,
    At,

    // Operators
    Plus,
    PlusEq,
    Minus,
    MinusEq,
    Star,
    StarEq,
    FwSlash,
    FwSlashEq,
    Percent,
    PercentEq,
    QuestionMark,
    Comma,
    Amp,
    AmpEq,
    AmpAmp,
    AmpAmpEq,
    Bar,
    BarEq,
    BarBar,
    BarBarEq,
    Caret,
    CaretEq,
    Bang,
    BangEq,
    Eq,
    EqEq,
    Lt,
    LtEq,
    LtLt,
    LtLtEq,
    Gt,
    GtEq,
    GtGt,
    GtGtEq,
    Dot,
    DotDot,
    DotDotDot,
    RightArrow,

    // Keywords
    If,
    Else,
    While,
    For,
    Break,
    Continue,
    Return,
    Let,
    Static,
    Fn,
    Import,
    Extern,
    Intrinsic,
    Pub,
    Mut,
    In,
    As,
    Struct,
    Union,
    Match,

    // Accessors
    Placeholder,
    Ident(Ustr),

    // Literals
    Nil,
    True,
    False,
    Int(i64),
    Float(f64),
    Str(Ustr),
    Char(char),

    // Misc
    Unknown(char),
    Eof,
}

impl From<&str> for TokenKind {
    fn from(s: &str) -> Self {
        use TokenKind::*;

        match s {
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
            "let" => Let,
            "static" => Static,
            "fn" => Fn,
            "import" => Import,
            "extern" => Extern,
            "intrinsic" => Intrinsic,
            "pub" => Pub,
            "mut" => Mut,
            "in" => In,
            "as" => As,
            "struct" => Struct,
            "union" => Union,
            "match" => Match,
            "_" => Placeholder,
            s => Ident(ustr(s)),
        }
    }
}

impl TokenKind {
    pub fn lexeme(&self) -> &str {
        use TokenKind::*;

        match self {
            At => "@",
            Semicolon => ";",
            Colon => ":",
            OpenParen => "(",
            CloseParen => ")",
            OpenCurly => "{",
            CloseCurly => "}",
            OpenBracket => "[",
            CloseBracket => "]",
            Plus => "+",
            PlusEq => "+=",
            Minus => "-",
            MinusEq => "-=",
            Star => "*",
            StarEq => "*=",
            FwSlash => "/",
            FwSlashEq => "/=",
            Percent => "%",
            PercentEq => "%=",
            QuestionMark => "?",
            Comma => ",",
            Amp => "&",
            AmpEq => "&=",
            AmpAmp => "&&",
            AmpAmpEq => "&&=",
            Bar => "|",
            BarEq => "|=",
            BarBar => "||",
            BarBarEq => "||=",
            Caret => "^",
            CaretEq => "^=",
            Bang => "!",
            BangEq => "!=",
            Eq => "=",
            EqEq => "==",
            Lt => "<",
            LtEq => "<=",
            LtLt => "<<",
            LtLtEq => "<<=",
            Gt => ">",
            GtEq => ">=",
            GtGt => ">>",
            GtGtEq => ">>=",
            Dot => ".",
            DotDot => "..",
            DotDotDot => "...",
            RightArrow => "->",
            If => "if",
            Else => "else",
            While => "while",
            For => "for",
            Break => "break",
            Continue => "continue",
            Return => "return",
            Let => "let",
            Static => "static",
            Fn => "fn",
            Import => "import",
            Extern => "extern",
            Intrinsic => "intrinsic",
            Pub => "pub",
            Mut => "mut",
            In => "in",
            As => "as",
            Struct => "struct",
            Union => "union",
            Match => "match",
            Placeholder => "_",
            Ident(_) => "identifier",
            Nil => "nil",
            True => "true",
            False => "false",
            Int(_) => "{integer}",
            Float(_) => "{float}",
            Str(_) => "{string}",
            Char(_) => "{char}",
            Unknown(_) => "?",
            Eof => "EOF",
        }
    }

    pub fn is_expr_start(&self) -> bool {
        use TokenKind::*;

        matches!(
            self,
            At | OpenParen
                | OpenCurly
                | OpenBracket
                | Plus
                | Minus
                | Star
                | QuestionMark
                | Amp
                | Bar
                | Bang
                | If
                | While
                | For
                | Break
                | Continue
                | Return
                | Let
                | Fn
                | Extern
                | Intrinsic
                | Pub
                | Struct
                | Union
                | Match
                | Placeholder
                | Ident(_)
                | Nil
                | True
                | False
                | Int(_)
                | Float(_)
                | Str(_)
                | Char(_)
        )
    }

    // pub fn is_expr_end(&self) -> bool {
    //     use TokenKind::*;

    //     matches!(
    //         self,
    //         Semicolon
    //             | CloseParen
    //             | CloseCurly
    //             | CloseBracket
    //             | QuestionMark
    //             | Break
    //             | Continue
    //             | Return
    //             | Placeholder
    //             | Ident(_)
    //             | Nil
    //             | True
    //             | False
    //             | Int(_)
    //             | Float(_)
    //             | Str(_)
    //             | Char(_)
    //             | Eof
    //     )
    // }
}
