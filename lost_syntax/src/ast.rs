use std::{fmt::Display, hash::Hash};

use crate::token::{TextRange, TokenKind};

#[derive(Clone, Debug, PartialEq)]
pub struct Source {
    pub items: Vec<Item>,
}

#[derive(Clone, Debug, PartialEq)]
pub enum Item {
    Class {
        ident: Ident,
        parent: Option<Ident>,
        methods: Vec<Item>,
    },
    Function {
        ident: Ident,
        args: Vec<Ident>,
        body: Vec<Item>,
    },
    LetStmt {
        ident: Ident,
        init: Option<Expr>,
    },
    ExprStmt(Expr),
    IfStmt {
        condition: Expr,
        if_item: Box<Item>,
        else_item: Option<Box<Item>>,
    },
    WhileStmt {
        condition: Expr,
        body: Box<Item>,
    },
    ReturnStmt(Expr),
    Block(Vec<Item>),
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum UnaryOp {
    Bang,
    Minus,
    Increment,
    Decrement,
}

impl UnaryOp {
    pub fn from_token(t: TokenKind) -> Self {
        match t {
            TokenKind::Bang => Self::Bang,
            TokenKind::Minus => Self::Minus,
            TokenKind::Increment => Self::Increment,
            TokenKind::Decrement => Self::Decrement,
            _ => unreachable!("non-unary operator cannot be passed to this function"),
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum BinOp {
    Slash,
    Star,
    Plus,
    Minus,
    Modulo,
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
            Self::Modulo => "%",
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
    pub fn from_token(t: TokenKind) -> Self {
        match t {
            TokenKind::Slash => Self::Slash,
            TokenKind::Star => Self::Star,
            TokenKind::Plus => Self::Plus,
            TokenKind::Minus => Self::Minus,
            TokenKind::Modulo => Self::Modulo,
            TokenKind::Greater => Self::Greater,
            TokenKind::GreaterEqual => Self::GreaterEqual,
            TokenKind::Less => Self::Less,
            TokenKind::LessEqual => Self::LessEqual,
            TokenKind::BangEqual => Self::BangEqual,
            TokenKind::EqualEqual => Self::EqualEqual,
            _ => unreachable!("non-binary operator cannot be passed to this function"),
        }
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
            TokenKind::And => Self::And,
            TokenKind::Or => Self::Or,
            _ => return None,
        };
        Some(op)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Ident {
    pub name: String,
    // Identifiers in different scopes may have the same name,
    // but are not identical since they refer to different variables.
    // The text range ensures that there is no collision between
    // these identfiers during resolution.
    pub range: TextRange,
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
        name: Ident,
        value: Box<Expr>,
    },
    Ident(Ident),
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
    Call {
        func: Box<Expr>,
        args: Vec<Expr>,
    },
    FieldGet {
        object: Box<Expr>,
        field: Ident,
    },
    FieldSet {
        object: Box<Expr>,
        field: Ident,
        value: Box<Expr>,
    },
    Super {
        super_: Ident,
        method: Ident,
    },
}
