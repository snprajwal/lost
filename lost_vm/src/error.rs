use std::fmt::Display;

pub type Error = String;

#[derive(Debug)]
pub enum ErrorMsg {
    EmptyStack,
    ExpectedStr,
    ExpectedNum,
    ExpectedNumOrStr,
}

impl Display for ErrorMsg {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(match self {
            Self::EmptyStack => "stack is empty for operation",
            Self::ExpectedStr => "expected string, found",
            Self::ExpectedNum => "expected number, found",
            Self::ExpectedNumOrStr => "expected both operands to be strings or numbers, found",
        })
    }
}

pub fn runtime_error(msg: ErrorMsg, ctx: impl Display) -> Error {
    format!("Runtime error: {} {}", msg, ctx)
}
