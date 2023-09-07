use std::fmt::Display;

pub type Error = String;

#[derive(Debug)]
pub enum ErrorMsg {
    // Lex errors
    UnexpectedChar,
    UnterminatedString,
    // Parse errors
    UnexpectedToken,
    MissingOpeningParen,
    MissingClosingParen,
    MissingOpeningBrace,
    MissingClosingBrace,
    MissingSemicolon,
    InvalidIdent,
    InvalidField,
    InvalidAssignment,
    // EOF
    EndOfStream,
}

impl Display for ErrorMsg {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(match self {
            Self::UnexpectedChar => "unexpected character",
            Self::UnterminatedString => "unterminated string",
            Self::UnexpectedToken => "unexpected token",
            Self::MissingOpeningParen => "missing opening parenthesis at",
            Self::MissingClosingParen => "missing closing parenthesis at",
            Self::MissingOpeningBrace => "missing opening brace at",
            Self::MissingClosingBrace => "missing closing brace at",
            Self::MissingSemicolon => "missing semicolon at",
            Self::InvalidIdent => "invalid identifier",
            Self::InvalidField => "invalid field",
            Self::InvalidAssignment => "invalid assignment target",
            Self::EndOfStream => "end of stream",
        })
    }
}
