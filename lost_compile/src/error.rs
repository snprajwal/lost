use std::fmt::Display;

use crate::types::Value;

#[derive(Debug)]
pub enum Exception {
    Error(String),
    Return(Value),
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
    ExpectedObject,
    ExpectedClass,
    InvalidCall,
    InvalidStrOp,
    TooManyArgs,
    TooFewArgs,
    GetConstructor,
    MisresolvedVar,
    UndefinedMember,
    UndefinedMethod,
    // Resolution errors
    NoScope,
    UndefinedVar,
    SelfInitialiser,
    SelfInherit,
    ReturnOutsideFunction,
    ThisOutsideMethod,
    SuperOutsideMethod,
    SuperWithoutParent,
}

impl Display for ErrorMsg {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(match self {
            Self::ExpectedNumber => "expected numeric operand, found",
            Self::ExpectedNumOrStr => "expected both operands to be numeric or string, found",
            Self::ExpectedIdent => "expected identifier, found",
            Self::ExpectedObject => "expected object, found",
            Self::ExpectedClass => "expected class, found",
            Self::InvalidCall => "cannot call non-callable value",
            Self::InvalidStrOp => "invalid string operation",
            Self::TooManyArgs => "too many arguments in function call",
            Self::TooFewArgs => "too few arguments in function call",
            Self::GetConstructor => "illegal to get constructor of class",
            Self::NoScope => "no scope present to resolve",
            Self::UndefinedVar => "undefined variable",
            Self::UndefinedMethod => "undefined class method",
            Self::MisresolvedVar => "misresolved variable",
            Self::UndefinedMember => "undefined object member",
            Self::SelfInitialiser => "cannot use self to initialise",
            Self::SelfInherit => "cannot inherit self in class",
            Self::ReturnOutsideFunction => "cannot return from outside a function",
            Self::ThisOutsideMethod => "cannot use `this` outside class methods",
            Self::SuperOutsideMethod => "cannot use `super` outside class methods",
            Self::SuperWithoutParent => "cannot use `super` without parent class",
        })
    }
}

pub fn runtime_error(msg: ErrorMsg, val: &str) -> Exception {
    Exception::Error(format!("Runtime error: {} {}", msg, val))
}

pub fn resolution_error(msg: ErrorMsg, val: &str) -> Exception {
    Exception::Error(
        format!("Resolution error: {} {}", msg, val)
            .trim()
            .to_string(),
    )
}
