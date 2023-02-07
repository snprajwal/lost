use std::fmt::Display;

pub type Error = String;

#[derive(Debug)]
pub enum ErrorMsg {
    // Lex errors
    UnexpectedChar,
    UnterminatedString,
    // Parse errors
    UnexpectedToken,
    MissingParen,
    // EOF
    EndOfStream,
}

impl Display for ErrorMsg {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(match self {
            Self::UnexpectedChar => "unexpected character",
            Self::UnterminatedString => "unterminated string",
            Self::UnexpectedToken => "unexpected token",
            Self::MissingParen => "missing closing parenthesis at",
            Self::EndOfStream => "end of stream",
        })
    }
}
