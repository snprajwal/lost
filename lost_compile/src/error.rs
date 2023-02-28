use std::fmt::Display;

pub type Error = String;

#[derive(Debug)]
pub enum ErrorMsg {
    // Runtime errors
    ExpectedNumber,
    ExpectedNumOrStr,
    InvalidStrOp,
    // Memory errors
    UndefinedVar,
}

impl Display for ErrorMsg {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(match self {
            Self::ExpectedNumber => "expected numeric operand",
            Self::ExpectedNumOrStr => "expected both operands to be numeric or string",
            Self::InvalidStrOp => "invalid operation on strings",
            Self::UndefinedVar => "undefined variable",
        })
    }
}

pub fn make(msg: ErrorMsg, val: String) -> Error {
    format!("Runtime error: {}, found {}", msg, val)
}
