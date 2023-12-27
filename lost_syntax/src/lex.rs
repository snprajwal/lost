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

    pub fn lex_all_sanitised(self) -> Result<Vec<Token>, Vec<Error>> {
        self.lex_all().map(|v| {
            v.into_iter()
                .filter(|t| !matches!(t.kind, TokenKind::WHITESPACE | TokenKind::COMMENT))
                .collect()
        })
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
                    '!' => Ok(self.advance_if_or('=', TokenKind::BANG_EQUAL, TokenKind::BANG)),
                    '=' => Ok(self.advance_if_or('=', TokenKind::EQUAL_EQUAL, TokenKind::EQUAL)),
                    '>' => {
                        Ok(self.advance_if_or('=', TokenKind::GREATER_EQUAL, TokenKind::GREATER))
                    }
                    '<' => Ok(if self.advance_if(|t| t == '=').is_some() {
                        self.make_token(TokenKind::LESS_EQUAL)
                    } else {
                        self.advance_if_or('-', TokenKind::INHERIT, TokenKind::LESS)
                    }),
                    '+' => Ok(self.advance_if_or('+', TokenKind::INCREMENT, TokenKind::PLUS)),
                    '-' => Ok(self.advance_if_or('-', TokenKind::DECREMENT, TokenKind::MINUS)),
                    '/' => Ok(self.lex_slash_or_comment()),
                    '"' => self.lex_string(),
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
        self.advance_if(|c| c == '"')
            .ok_or_else(|| self.error(ErrorMsg::UnterminatedString))?;
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

    fn advance_if_or(
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

#[cfg(test)]
mod tests {
    use super::*;

    #[inline]
    fn lex_test(input: &str, expected: Vec<(TokenKind, &str)>) {
        let lexer = Lexer::new(input);
        expected
            .into_iter()
            .zip(lexer.lex_all_sanitised().unwrap())
            .for_each(|((k, l), t)| {
                assert_eq!(t.kind, k);
                assert_eq!(t.lexeme, l);
            });
    }

    #[test]
    fn keywords() {
        lex_test(
            r#"
            and class else false fn for if null
            or return super this true let while
            "#,
            vec![
                (TokenKind::AND, "and"),
                (TokenKind::CLASS, "class"),
                (TokenKind::ELSE, "else"),
                (TokenKind::FALSE, "false"),
                (TokenKind::FN, "fn"),
                (TokenKind::FOR, "for"),
                (TokenKind::IF, "if"),
                (TokenKind::NULL, "null"),
                (TokenKind::OR, "or"),
                (TokenKind::RETURN, "return"),
                (TokenKind::SUPER, "super"),
                (TokenKind::THIS, "this"),
                (TokenKind::TRUE, "true"),
                (TokenKind::LET, "let"),
                (TokenKind::WHILE, "while"),
            ],
        );
    }

    #[test]
    fn literal() {
        lex_test(
            "10 3.14 foo bar \"Hello there\"",
            vec![
                (TokenKind::NUMBER, "10"),
                (TokenKind::NUMBER, "3.14"),
                (TokenKind::IDENT, "foo"),
                (TokenKind::IDENT, "bar"),
                (TokenKind::STRING, "Hello there"),
            ],
        );
    }

    #[test]
    fn cmp() {
        lex_test(
            "< > <= >= == !=",
            vec![
                (TokenKind::LESS, "<"),
                (TokenKind::GREATER, ">"),
                (TokenKind::LESS_EQUAL, "<="),
                (TokenKind::GREATER_EQUAL, ">="),
                (TokenKind::EQUAL_EQUAL, "=="),
                (TokenKind::BANG_EQUAL, "!="),
            ],
        );
    }

    #[test]
    fn arithmetic() {
        lex_test(
            "! + - * / =",
            vec![
                (TokenKind::BANG, "!"),
                (TokenKind::PLUS, "+"),
                (TokenKind::MINUS, "-"),
                (TokenKind::STAR, "*"),
                (TokenKind::SLASH, "/"),
                (TokenKind::EQUAL, "="),
            ],
        );
    }

    #[test]
    fn symbols() {
        lex_test(
            ". , ; () {}",
            vec![
                (TokenKind::DOT, "."),
                (TokenKind::COMMA, ","),
                (TokenKind::SEMICOLON, ";"),
                (TokenKind::LPAREN, "("),
                (TokenKind::RPAREN, ")"),
                (TokenKind::LBRACE, "{"),
                (TokenKind::RBRACE, "}"),
            ],
        );
    }

    #[test]
    fn whitespace() {
        lex_test(
            " \t\r\n",
            vec![
                (TokenKind::WHITESPACE, " "),
                (TokenKind::WHITESPACE, "\t"),
                (TokenKind::WHITESPACE, "\r"),
                (TokenKind::WHITESPACE, "\n"),
            ],
        );
    }

    #[test]
    fn comment() {
        let input = "// Hello there";
        let token = Lexer::new(input).lex().unwrap();
        assert_eq!(token.kind, TokenKind::COMMENT);
        assert_eq!(token.lexeme, input);
    }

    #[test]
    fn eof() {
        let input = "";
        let token = Lexer::new(input).lex().unwrap();
        assert_eq!(token.kind, TokenKind::EOF);
        assert_eq!(token.lexeme, "end of file");
    }

    #[test]
    fn unexpected_char() {
        let input = "he@#llo";
        let mut lexer = Lexer::new(input);
        // Lex `he` as an ident
        lexer.lex().unwrap();
        assert_eq!(
            lexer.lex().unwrap_err(),
            format!("Lex error at line 1: {} @", ErrorMsg::UnexpectedChar)
        );
    }

    #[test]
    fn unterminated_string() {
        let input = "\"Hello there";
        let err = Lexer::new(input).lex().unwrap_err();
        assert_eq!(
            err,
            format!(
                "Lex error at line 1: {} {}",
                ErrorMsg::UnterminatedString,
                &input[1..]
            )
        );
    }

    #[test]
    fn end_of_stream() {
        let input = "";
        let mut lexer = Lexer::new(input);
        // Lex the EOF
        lexer.lex().unwrap();
        assert_eq!(
            lexer.lex().unwrap_err(),
            format!("Lex error: {}", ErrorMsg::EndOfStream.to_string())
        );
    }
}
