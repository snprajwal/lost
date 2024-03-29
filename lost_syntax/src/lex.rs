use crate::{
    error::{Error, ErrorMsg},
    token::{Location, Token, TokenKind},
};
use std::{iter::Peekable, str::Chars};

#[derive(Debug)]
pub struct Lexer<'a> {
    source: String,
    stream: Peekable<Chars<'a>>,
    start: usize,
    current: usize,
    line: usize,
    column: usize,
}

impl<'a> Lexer<'a> {
    pub fn new(source: &'a str) -> Self {
        Self {
            source: source.to_string(),
            stream: source.chars().peekable(),
            start: 0,
            current: 0,
            line: 1,
            column: 0,
        }
    }

    pub fn lex_all_sanitised(self) -> Result<Vec<Token>, Vec<Error>> {
        self.lex_all().map(|v| {
            v.into_iter()
                .filter(|t| !matches!(t.kind, TokenKind::Whitespace | TokenKind::Comment))
                .collect()
        })
    }

    pub fn lex_all(mut self) -> Result<Vec<Token>, Vec<Error>> {
        let mut tokens: Vec<Token> = Vec::default();
        let mut errors: Vec<Error> = Vec::default();
        loop {
            match self.lex() {
                Ok(t) => {
                    if t.kind == TokenKind::Eof {
                        break;
                    }
                    tokens.push(t);
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
                    '!' => Ok(self.advance_if_or('=', TokenKind::BangEqual, TokenKind::Bang)),
                    '=' => Ok(self.advance_if_or('=', TokenKind::EqualEqual, TokenKind::Equal)),
                    '>' => Ok(self.advance_if_or('=', TokenKind::GreaterEqual, TokenKind::Greater)),
                    '<' => Ok(if self.advance_if(|t| t == '=').is_some() {
                        self.make_token(TokenKind::LessEqual)
                    } else {
                        self.advance_if_or('-', TokenKind::Inherit, TokenKind::Less)
                    }),
                    '+' => Ok(self.advance_if_or('+', TokenKind::Increment, TokenKind::Plus)),
                    '-' => Ok(self.advance_if_or('-', TokenKind::Decrement, TokenKind::Minus)),
                    '/' => Ok(self.lex_slash_or_comment()),
                    '"' => self.lex_string(),
                    _ => {
                        if let Some(t) = TokenKind::from_char(c) {
                            // If it is a newline, increment the current line count
                            if c == '\n' {
                                self.line += 1;
                                self.column = 0;
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
                        TokenKind::Eof,
                        Location {
                            start: self.current,
                            end: self.current,
                            line: self.line,
                            column: self.column,
                        },
                        "EOF".to_string(),
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
            self.make_token(TokenKind::Ident)
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
        Ok(self.make_token(TokenKind::Number))
    }

    fn lex_string(&mut self) -> Result<Token, Error> {
        // Consume the opening apostrophe(")
        self.start += 1;
        while let Some(c) = self.advance_if(|c| c != '"') {
            if c == '\n' {
                return Err(self.error(ErrorMsg::UnterminatedString));
            }
        }
        let token = self.make_token(TokenKind::Str);
        // Consume the closing apostrophe(")
        self.advance_if(|c| c == '"')
            .ok_or_else(|| self.error(ErrorMsg::UnterminatedString))?;
        Ok(token)
    }

    fn lex_slash_or_comment(&mut self) -> Token {
        if self.advance_if(|c| c == '/').is_some() {
            self.advance_while(|c| c != '\n');
            self.make_token(TokenKind::Comment)
        } else {
            self.make_token(TokenKind::Slash)
        }
    }

    fn make_token(&self, kind: TokenKind) -> Token {
        Token::new(kind, self.location(), self.lexeme_from_range())
    }

    fn lexeme_from_range(&self) -> String {
        self.source[self.start..self.current].to_string()
    }

    fn location(&self) -> Location {
        Location {
            start: self.start,
            end: self.current,
            line: self.line,
            column: self.column,
        }
    }

    fn advance(&mut self) -> Option<char> {
        self.current += 1;
        self.column += 1;
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
            "Lex error ({}:{}): {} {}",
            self.line,
            self.column,
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
                (TokenKind::And, "and"),
                (TokenKind::Class, "class"),
                (TokenKind::Else, "else"),
                (TokenKind::False, "false"),
                (TokenKind::Fn_, "fn"),
                (TokenKind::For, "for"),
                (TokenKind::If, "if"),
                (TokenKind::Null, "null"),
                (TokenKind::Or, "or"),
                (TokenKind::Return, "return"),
                (TokenKind::Super, "super"),
                (TokenKind::This, "this"),
                (TokenKind::True, "true"),
                (TokenKind::Let, "let"),
                (TokenKind::While, "while"),
            ],
        );
    }

    #[test]
    fn literal() {
        lex_test(
            "10 3.14 foo bar \"Hello there\"",
            vec![
                (TokenKind::Number, "10"),
                (TokenKind::Number, "3.14"),
                (TokenKind::Ident, "foo"),
                (TokenKind::Ident, "bar"),
                (TokenKind::Str, "Hello there"),
            ],
        );
    }

    #[test]
    fn cmp() {
        lex_test(
            "< > <= >= == !=",
            vec![
                (TokenKind::Less, "<"),
                (TokenKind::Greater, ">"),
                (TokenKind::LessEqual, "<="),
                (TokenKind::GreaterEqual, ">="),
                (TokenKind::EqualEqual, "=="),
                (TokenKind::BangEqual, "!="),
            ],
        );
    }

    #[test]
    fn arithmetic() {
        lex_test(
            "! + - * / =",
            vec![
                (TokenKind::Bang, "!"),
                (TokenKind::Plus, "+"),
                (TokenKind::Minus, "-"),
                (TokenKind::Star, "*"),
                (TokenKind::Slash, "/"),
                (TokenKind::Equal, "="),
            ],
        );
    }

    #[test]
    fn symbols() {
        lex_test(
            ". , ; () {}",
            vec![
                (TokenKind::Dot, "."),
                (TokenKind::Comma, ","),
                (TokenKind::Semicolon, ";"),
                (TokenKind::LParen, "("),
                (TokenKind::RParen, ")"),
                (TokenKind::LBrace, "{"),
                (TokenKind::RBrace, "}"),
            ],
        );
    }

    #[test]
    fn whitespace() {
        lex_test(
            " \t\r\n",
            vec![
                (TokenKind::Whitespace, " "),
                (TokenKind::Whitespace, "\t"),
                (TokenKind::Whitespace, "\r"),
                (TokenKind::Whitespace, "\n"),
            ],
        );
    }

    #[test]
    fn comment() {
        let input = "// Hello there";
        let token = Lexer::new(input).lex().unwrap();
        assert_eq!(token.kind, TokenKind::Comment);
        assert_eq!(token.lexeme, input);
    }

    #[test]
    fn eof() {
        let input = "";
        let token = Lexer::new(input).lex().unwrap();
        assert_eq!(token.kind, TokenKind::Eof);
        assert_eq!(token.lexeme, "EOF");
    }

    #[test]
    fn unexpected_char() {
        let input = "he@#llo";
        let mut lexer = Lexer::new(input);
        // Lex `he` as an ident
        lexer.lex().unwrap();
        assert_eq!(
            lexer.lex().unwrap_err(),
            format!("Lex error (1:3): {} @", ErrorMsg::UnexpectedChar)
        );
    }

    #[test]
    fn unterminated_string() {
        let input = "\"Hello there";
        let err = Lexer::new(input).lex().unwrap_err();
        assert_eq!(
            err,
            format!(
                "Lex error (1:12): {} {}",
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
            format!("Lex error: {}", ErrorMsg::EndOfStream)
        );
    }
}
