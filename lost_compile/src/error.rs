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
    // Memory errors
    UndefinedVar,
    UndefinedMember,
}

impl Display for ErrorMsg {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(match self {
            Self::ExpectedNumber => "expected numeric operand",
            Self::ExpectedNumOrStr => "expected both operands to be numeric or string",
            Self::ExpectedIdent => "expected identifier",
            Self::InvalidStrOp => "invalid operation on strings",
            Self::InvalidCallExpr => "cannot call this function",
            Self::InvalidObject => "cannot use this as an object",
            Self::TooManyArgs => "too many arguments in function call",
            Self::TooFewArgs => "too few arguments in function call",
            Self::UndefinedVar => "undefined variable",
            Self::UndefinedMember => "undefined member",
        })
    }
}

pub fn make(msg: ErrorMsg, val: String) -> Exception {
    Exception::Error(format!("Runtime error: {}, found {}", msg, val))
}
