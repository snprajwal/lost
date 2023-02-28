use crate::{
    error::{Error, ErrorMsg},
    token::{TextRange, Token, TokenKind},
};
use std::{iter::Peekable, str::Chars};

#[derive(Debug)]
pub struct Lexer<'a> {
    source: String,
    stream: Peekable<Chars<'a>>,
    line: usize,
    start: usize,
    current: usize,
}

impl<'a> Lexer<'a> {
    pub fn new(source: &'a str) -> Self {
        Self {
            source: source.to_string(),
            stream: source.chars().peekable(),
            line: 0,
            start: 0,
            current: 0,
        }
    }

    pub fn lex_all(mut self) -> Result<Vec<Token>, Vec<Error>> {
        let mut tokens: Vec<Token> = Vec::default();
        let mut errors: Vec<Error> = Vec::default();
        loop {
            match self.lex() {
                Ok(t) => {
                    if t.kind == TokenKind::EOF {
                        break;
                    } else {
                        tokens.push(t);
                    }
                }
                Err(e) => errors.push(e),
            }
        }
        if errors.is_empty() {
            Ok(tokens)
        } else {
            Err(errors)
        }
    }

    pub fn lex(&mut self) -> Result<Token, Error> {
        self.start = self.current;
        match self.advance() {
            Some(c) => {
                match c {
                    '!' => {
                        Ok(self.lookahead_for_token('=', TokenKind::BANG_EQUAL, TokenKind::EQUAL))
                    }
                    '=' => {
                        Ok(self.lookahead_for_token('=', TokenKind::EQUAL_EQUAL, TokenKind::EQUAL))
                    }
                    '>' => Ok(self.lookahead_for_token(
                        '=',
                        TokenKind::GREATER_EQUAL,
                        TokenKind::GREATER,
                    )),
                    '<' => {
                        Ok(self.lookahead_for_token('=', TokenKind::LESS_EQUAL, TokenKind::LESS))
                    }
                    '"' => self.lex_string(),
                    '/' => Ok(self.lex_slash_or_comment()),
                    _ => {
                        if let Some(t) = TokenKind::from_char(c) {
                            // If it is a newline, increment the current line count
                            if c == '\n' {
                                self.line += 1;
                            }
                            Ok(self.make_token(t))
                        } else if c.is_ascii_alphabetic() {
                            Ok(self.lex_ident())
                        } else if c.is_ascii_digit() {
                            self.lex_number()
                        } else {
                            Err(self.error(ErrorMsg::UnexpectedChar))
                        }
                    }
                }
            }
            _ => {
                if self.start == self.source.len() {
                    Ok(Token::new(
                        TokenKind::EOF,
                        TextRange {
                            start: self.current,
                            end: self.current,
                        },
                        self.line,
                        "end of file".to_string(),
                    ))
                } else {
                    Err(format!("Lex error: {}", ErrorMsg::EndOfStream))
                }
            }
        }
    }

    fn lex_ident(&mut self) -> Token {
        self.advance_while(|c| c.is_alphanumeric() || c == '_');
        if let Some(t) = TokenKind::from_keyword(&self.lexeme_from_range()) {
            self.make_token(t)
        } else {
            self.make_token(TokenKind::IDENT)
        }
    }

    fn lex_number(&mut self) -> Result<Token, Error> {
        // Consume numbers
        self.advance_while(|c| c.is_ascii_digit());
        // Check if dot is present
        if self.advance_if(|c| c == '.').is_some() {
            // Ensure the next character is a number
            if self.advance_while(|c| c.is_ascii_digit()).is_none() {
                self.start = self.current - 1;
                return Err(self.error(ErrorMsg::UnexpectedChar));
            }
        }
        Ok(self.make_token(TokenKind::NUMBER))
    }

    fn lex_string(&mut self) -> Result<Token, Error> {
        // Consume the opening apostrophe(")
        self.start += 1;
        while let Some(c) = self.advance_if(|c| c != '"') {
            if c == '\n' {
                return Err(self.error(ErrorMsg::UnterminatedString));
            }
        }
        let token = self.make_token(TokenKind::STRING);
        // Consume the closing apostrophe(")
        self.advance();
        Ok(token)
    }

    fn lex_slash_or_comment(&mut self) -> Token {
        if self.advance_if(|c| c == '/').is_some() {
            self.advance_while(|c| c != '\n');
            self.make_token(TokenKind::COMMENT)
        } else {
            self.make_token(TokenKind::SLASH)
        }
    }

    fn make_token(&self, kind: TokenKind) -> Token {
        Token::new(kind, self.text_range(), self.line, self.lexeme_from_range())
    }

    fn lexeme_from_range(&self) -> String {
        self.source[self.start..self.current].to_string()
    }

    fn text_range(&self) -> TextRange {
        TextRange {
            start: self.start,
            end: self.current,
        }
    }

    fn advance(&mut self) -> Option<char> {
        self.current += 1;
        self.stream.next()
    }

    fn advance_if<F>(&mut self, cond: F) -> Option<char>
    where
        F: FnOnce(char) -> bool,
    {
        if self.stream.peek().filter(|&&c| cond(c)).is_some() {
            self.advance()
        } else {
            None
        }
    }

    fn advance_while<F>(&mut self, cond: F) -> Option<usize>
    where
        F: Fn(char) -> bool,
    {
        let mut count: usize = 0;
        while self.stream.peek().filter(|&&c| cond(c)).is_some() {
            count += 1;
            self.advance();
        }
        count.ne(&0).then_some(count)
    }

    fn lookahead_for_token(
        &mut self,
        match_char: char,
        if_match: TokenKind,
        no_match: TokenKind,
    ) -> Token {
        if self.advance_if(|c| c == match_char).is_some() {
            self.make_token(if_match)
        } else {
            self.make_token(no_match)
        }
    }

    fn error(&self, msg: ErrorMsg) -> Error {
        format!(
            "Lex error at line {}: {} {}",
            self.line + 1,
            msg,
            self.lexeme_from_range()
        )
    }
}
