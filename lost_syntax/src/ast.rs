use std::fmt::Display;

use crate::token::{TextRange, TokenKind};

#[derive(Debug)]
pub struct AstNode {
    pub range: TextRange,
    pub expr: Expr,
}

#[derive(Clone, Debug)]
pub enum UnaryOp {
    Bang,
    Minus,
}

impl UnaryOp {
    pub fn from_token(t: TokenKind) -> Option<Self> {
        let op = match t {
            TokenKind::BANG => Self::Bang,
            TokenKind::MINUS => Self::Minus,
            _ => return None,
        };
        Some(op)
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum BinOp {
    Slash,
    Star,
    Plus,
    Minus,
    Greater,
    GreaterEqual,
    Less,
    LessEqual,
    BangEqual,
    EqualEqual,
}

impl Display for BinOp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(match self {
            Self::Slash => "/",
            Self::Star => "*",
            Self::Plus => "+",
            Self::Minus => "-",
            Self::Greater => ">",
            Self::GreaterEqual => ">=",
            Self::Less => "<",
            Self::LessEqual => "<=",
            Self::BangEqual => "!=",
            Self::EqualEqual => "==",
        })
    }
}

impl BinOp {
    pub fn from_token(t: TokenKind) -> Option<Self> {
        let op = match t {
            TokenKind::SLASH => Self::Slash,
            TokenKind::STAR => Self::Star,
            TokenKind::PLUS => Self::Plus,
            TokenKind::MINUS => Self::Minus,
            TokenKind::GREATER => Self::Greater,
            TokenKind::GREATER_EQUAL => Self::GreaterEqual,
            TokenKind::LESS => Self::Less,
            TokenKind::LESS_EQUAL => Self::LessEqual,
            TokenKind::BANG_EQUAL => Self::BangEqual,
            TokenKind::EQUAL_EQUAL => Self::EqualEqual,
            _ => return None,
        };
        Some(op)
    }
}

#[derive(Debug)]
pub enum LogicalOp {
    And,
    Or,
}

impl LogicalOp {
    pub fn from_token(t: TokenKind) -> Option<Self> {
        let op = match t {
            TokenKind::AND => Self::And,
            TokenKind::OR => Self::Or,
            _ => return None,
        };
        Some(op)
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum Literal {
    Number(f64),
    Str(String),
    Boolean(bool),
    Null,
}

impl Display for Literal {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(&match self {
            Self::Null => "".to_string(),
            Self::Number(n) => n.to_string(),
            Self::Str(s) => s.to_owned(),
            Self::Boolean(b) => b.to_string(),
        })
    }
}

#[derive(Clone, Debug)]
pub enum Expr {
    Literal(Literal),
    Unary {
        op: UnaryOp,
        expr: Box<Expr>,
    },
    Binary {
        lhs: Box<Expr>,
        op: BinOp,
        rhs: Box<Expr>,
    },
    Group(Box<Expr>),
}
