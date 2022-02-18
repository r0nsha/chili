use std::fmt::Display;

use chilic_token::TokenType;

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum BinaryOp {
    Add,
    Sub,
    Mul,
    Div,
    Rem,
    Eq,
    NEq,
    Lt,
    LtEq,
    Gt,
    GtEq,
    And,
    Or,
    Shl,
    Shr,
    BitwiseAnd,
    BitwiseOr,
    BitwiseXor,
}

impl Display for BinaryOp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use BinaryOp::*;
        write!(
            f,
            "{}",
            match self {
                Add => "+",
                Sub => "-",
                Mul => "*",
                Div => "/",
                Rem => "%",
                Eq => "==",
                NEq => "!=",
                Lt => "<",
                LtEq => "<=",
                Gt => ">",
                GtEq => ">=",
                And => "&&",
                Or => "||",
                Shl => "<<",
                Shr => ">>",
                BitwiseAnd => "&",
                BitwiseOr => "|",
                BitwiseXor => "^",
            }
        )
    }
}

impl From<TokenType> for BinaryOp {
    fn from(token_type: TokenType) -> Self {
        use TokenType::*;
        match token_type {
            Plus | PlusEq => BinaryOp::Add,
            Minus | MinusEq => BinaryOp::Sub,
            Star | StarEq => BinaryOp::Mul,
            FwSlash | FwSlashEq => BinaryOp::Div,
            Percent | PercentEq => BinaryOp::Rem,
            EqEq => BinaryOp::Eq,
            BangEq => BinaryOp::NEq,
            Lt => BinaryOp::Lt,
            LtEq => BinaryOp::LtEq,
            Gt => BinaryOp::Gt,
            GtEq => BinaryOp::GtEq,
            AmpAmp | AmpAmpEq => BinaryOp::And,
            BarBar | BarBarEq => BinaryOp::Or,
            LtLt | LtLtEq => BinaryOp::Shl,
            GtGt | GtGtEq => BinaryOp::Shr,
            Amp | AmpEq => BinaryOp::BitwiseAnd,
            Bar | BarEq => BinaryOp::BitwiseOr,
            Caret | CaretEq => BinaryOp::BitwiseXor,
            _ => panic!("{} is not a binary op", token_type),
        }
    }
}

#[derive(strum_macros::IntoStaticStr, Debug, PartialEq, Clone, Copy)]
pub enum UnaryOp {
    Ref(bool),
    Deref,
    Neg,
    Plus,
    Not,
    BitwiseNot,
}

impl Display for UnaryOp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                UnaryOp::Ref(is_mutable) =>
                    format!("&{}", if *is_mutable { "mut " } else { "" }),
                UnaryOp::Deref => "*".to_string(),
                UnaryOp::Neg => "-".to_string(),
                UnaryOp::Plus => "+".to_string(),
                UnaryOp::Not => "!".to_string(),
                UnaryOp::BitwiseNot => "~".to_string(),
            }
        )
    }
}

impl From<TokenType> for UnaryOp {
    fn from(token_type: TokenType) -> Self {
        use TokenType::*;
        match token_type {
            Amp => UnaryOp::Ref(false),
            Minus => UnaryOp::Neg,
            Plus => UnaryOp::Plus,
            Bang => UnaryOp::Not,
            Tilde => UnaryOp::BitwiseNot,
            _ => panic!("{} is not a unary op", token_type),
        }
    }
}
