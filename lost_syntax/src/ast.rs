use std::fmt::Display;

use crate::token::TokenKind;

#[derive(Debug, PartialEq)]
pub struct Source {
    pub items: Vec<Item>,
}

#[derive(Clone, Debug, PartialEq)]
pub enum Item {
    LetStmt {
        name: Literal,
        init: Option<Expr>,
    },
    ExprStmt(Expr),
    PrintStmt(Expr),
    IfStmt {
        condition: Expr,
        if_item: Box<Item>,
        else_item: Option<Box<Item>>,
    },
    WhileStmt {
        condition: Expr,
        body: Box<Item>,
    },
    Block(Vec<Item>),
}

#[derive(Clone, Debug, PartialEq, Eq)]
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

#[derive(Clone, Debug, PartialEq, Eq)]
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
    Ident(String),
    Number(f64),
    Str(String),
    Boolean(bool),
    Null,
}

impl Display for Literal {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(&match self {
            Self::Ident(name) => name.to_owned(),
            Self::Number(n) => n.to_string(),
            Self::Str(s) => s.to_owned(),
            Self::Boolean(b) => b.to_string(),
            Self::Null => "null".to_string(),
        })
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum Expr {
    Assignment {
        name: Literal,
        value: Box<Expr>,
    },
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
    Logical {
        lhs: Box<Expr>,
        op: LogicalOp,
        rhs: Box<Expr>,
    },
    Group(Box<Expr>),
}
