use std::{fmt::Display, ops::AddAssign};

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub struct Location {
    pub start: usize,
    pub end: usize,
    pub line: usize,
    pub column: usize,
}

impl AddAssign for Location {
    fn add_assign(&mut self, rhs: Self) {
        self.start += rhs.start;
        self.end += rhs.end;
    }
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum TokenKind {
    // Symbols
    LParen,
    RParen,
    LBrace,
    RBrace,
    Comma,
    Dot,
    Semicolon,
    // Arithmetic
    Bang,
    Minus,
    Plus,
    Slash,
    Star,
    Modulo,
    Increment,
    Decrement,
    // Comparisons
    BangEqual,
    EqualEqual,
    Greater,
    GreaterEqual,
    Less,
    LessEqual,
    // Literals
    Ident,
    Str,
    Number,
    // Keywords
    And,
    Class,
    Else,
    False,
    Fn_,
    For,
    If,
    Null,
    Or,
    Return,
    Super,
    This,
    True,
    Let,
    While,
    // Miscellaneous tokens
    Equal,
    Inherit,
    Comment,
    Whitespace,
    Eof,
}

impl TokenKind {
    pub fn from_char(c: char) -> Option<Self> {
        let token = match c {
            '(' => Self::LParen,
            ')' => Self::RParen,
            '{' => Self::LBrace,
            '}' => Self::RBrace,
            ',' => Self::Comma,
            '.' => Self::Dot,
            '-' => Self::Minus,
            '+' => Self::Plus,
            ';' => Self::Semicolon,
            '*' => Self::Star,
            '%' => Self::Modulo,
            '=' => Self::Equal,
            '>' => Self::Greater,
            '<' => Self::Less,
            ' ' | '\t' | '\r' | '\n' => Self::Whitespace,
            _ => return None,
        };
        Some(token)
    }
    pub fn from_keyword(kw: &str) -> Option<Self> {
        let token = match kw {
            "and" => Self::And,
            "class" => Self::Class,
            "else" => Self::Else,
            "false" => Self::False,
            "fn" => Self::Fn_,
            "for" => Self::For,
            "if" => Self::If,
            "null" => Self::Null,
            "or" => Self::Or,
            "return" => Self::Return,
            "super" => Self::Super,
            "this" => Self::This,
            "true" => Self::True,
            "let" => Self::Let,
            "while" => Self::While,
            _ => return None,
        };
        Some(token)
    }
}

#[derive(Clone, Debug)]
pub struct Token {
    pub kind: TokenKind,
    pub loc: Location,
    pub lexeme: String,
}

impl Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(&self.lexeme)
    }
}

impl Token {
    pub fn new(kind: TokenKind, loc: Location, lexeme: String) -> Self {
        Self { kind, loc, lexeme }
    }
}
