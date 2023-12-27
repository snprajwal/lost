use std::{fmt::Display, ops::AddAssign};

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub struct TextRange {
    pub start: usize,
    pub end: usize,
}

impl AddAssign for TextRange {
    fn add_assign(&mut self, rhs: Self) {
        self.start += rhs.start;
        self.end += rhs.end;
    }
}

/// The enum variants are in SCREAMING_SNAKE_CASE as they technically
/// represent constants, but Rust does not allow const enum variants.
#[allow(nonstandard_style)]
#[derive(Copy, Clone, Debug, PartialEq, Eq)]
#[repr(u8)]
pub enum TokenKind {
    // Symbols
    LPAREN,
    RPAREN,
    LBRACE,
    RBRACE,
    COMMA,
    DOT,
    SEMICOLON,
    // Arithmetic
    BANG,
    MINUS,
    PLUS,
    SLASH,
    STAR,
    MODULO,
    INCREMENT,
    DECREMENT,
    // Comparisons
    BANG_EQUAL,
    EQUAL_EQUAL,
    GREATER,
    GREATER_EQUAL,
    LESS,
    LESS_EQUAL,
    // Literals
    IDENT,
    STRING,
    NUMBER,
    // Keywords
    AND,
    CLASS,
    ELSE,
    FALSE,
    FN,
    FOR,
    IF,
    NULL,
    OR,
    RETURN,
    SUPER,
    THIS,
    TRUE,
    LET,
    WHILE,
    // Miscellaneous tokens
    EQUAL,
    INHERIT,
    COMMENT,
    WHITESPACE,
    EOF,
}

impl TokenKind {
    pub fn from_char(c: char) -> Option<Self> {
        let token = match c {
            '(' => Self::LPAREN,
            ')' => Self::RPAREN,
            '{' => Self::LBRACE,
            '}' => Self::RBRACE,
            ',' => Self::COMMA,
            '.' => Self::DOT,
            '-' => Self::MINUS,
            '+' => Self::PLUS,
            ';' => Self::SEMICOLON,
            '*' => Self::STAR,
            '%' => Self::MODULO,
            '=' => Self::EQUAL,
            '>' => Self::GREATER,
            '<' => Self::LESS,
            ' ' | '\t' | '\r' | '\n' => Self::WHITESPACE,
            _ => return None,
        };
        Some(token)
    }
    pub fn from_keyword(kw: &str) -> Option<Self> {
        let token = match kw {
            "and" => Self::AND,
            "class" => Self::CLASS,
            "else" => Self::ELSE,
            "false" => Self::FALSE,
            "fn" => Self::FN,
            "for" => Self::FOR,
            "if" => Self::IF,
            "null" => Self::NULL,
            "or" => Self::OR,
            "return" => Self::RETURN,
            "super" => Self::SUPER,
            "this" => Self::THIS,
            "true" => Self::TRUE,
            "let" => Self::LET,
            "while" => Self::WHILE,
            _ => return None,
        };
        Some(token)
    }
}

#[derive(Clone, Debug)]
pub struct Token {
    pub kind: TokenKind,
    pub range: TextRange,
    pub line: usize,
    pub lexeme: String,
}

impl Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(&self.lexeme)
    }
}

impl Token {
    pub fn new(kind: TokenKind, range: TextRange, line: usize, lexeme: String) -> Self {
        Self {
            kind,
            range,
            line,
            lexeme,
        }
    }
}
