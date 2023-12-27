use std::fmt::Display;

use crate::types::Type;

#[derive(Debug)]
pub enum Exception {
    Error(String),
    Return(Type),
}

impl Display for Exception {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(&match self {
            Self::Return(val) => val.to_string(),
            Self::Error(e) => e.to_owned(),
        })
    }
}

#[derive(Debug)]
pub enum ErrorMsg {
    // Runtime errors
    ExpectedNumber,
    ExpectedNumOrStr,
    ExpectedIdent,
    InvalidStrOp,
    InvalidCallExpr,
    InvalidObject,
    TooManyArgs,
    TooFewArgs,
    GetConstructor,
    // Resolution errors
    NoScope,
    UndefinedVar,
    MisresolvedVar,
    UndefinedMember,
    SelfIntialiser,
    ReturnOutsideFunction,
    ThisOutsideMethod,
}

impl Display for ErrorMsg {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(match self {
            Self::ExpectedNumber => "expected numeric operand, found",
            Self::ExpectedNumOrStr => "expected both operands to be numeric or string, found",
            Self::ExpectedIdent => "expected identifier, found",
            Self::InvalidStrOp => "invalid string operation",
            Self::InvalidCallExpr => "cannot call non-function value",
            Self::InvalidObject => "invalid object, found",
            Self::TooManyArgs => "too many arguments in function call",
            Self::TooFewArgs => "too few arguments in function call",
            Self::GetConstructor => "illegal to get constructor of class",
            Self::NoScope => "no scope present to resolve",
            Self::UndefinedVar => "undefined variable",
            Self::MisresolvedVar => "misresolved variable",
            Self::UndefinedMember => "undefined object member",
            Self::SelfIntialiser => "cannot use self to initialise",
            Self::ReturnOutsideFunction => "cannot return from outside a function",
            Self::ThisOutsideMethod => "cannot use `this` variable outside class methods",
        })
    }
}

pub fn runtime_error(msg: ErrorMsg, val: String) -> Exception {
    Exception::Error(format!("Runtime error: {} {}", msg, val))
}

pub fn resolution_error(msg: ErrorMsg, val: String) -> Exception {
    Exception::Error(
        format!("Resolution error: {} {}", msg, val)
            .trim()
            .to_string(),
    )
}
