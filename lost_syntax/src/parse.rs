use std::{iter::Peekable, slice::Iter};

use crate::{
    ast::{self, Expr, Item, Literal, Source},
    error::{Error, ErrorMsg},
    token::{Token, TokenKind},
};

#[derive(Debug)]
pub struct Parser<'a> {
    stream: Peekable<Iter<'a, Token>>,
}

impl<'a> Parser<'a> {
    pub fn new(stream: &'a [Token]) -> Self {
        Self {
            stream: stream.iter().peekable(),
        }
    }

    pub fn parse_all(mut self) -> Result<Source, Vec<Error>> {
        let mut items: Vec<Item> = Vec::default();
        let mut errors: Vec<Error> = Vec::default();
        while self.stream.peek().is_some() {
            match self.parse() {
                Ok(stmt) => items.push(stmt),
                Err(e) => {
                    errors.push(e);
                    self.sync();
                }
            }
        }

        errors.is_empty().then(|| Source { items }).ok_or(errors)
    }

    pub fn parse(&mut self) -> Result<Item, Error> {
        self.parse_item()
    }

    pub fn parse_item(&mut self) -> Result<Item, Error> {
        match self.stream.peek() {
            Some(&t) => {
                let item = match t.kind {
                    TokenKind::LET => self.parse_let_stmt(),
                    TokenKind::LBRACE => return self.parse_block(),
                    TokenKind::IF => return self.parse_if_stmt(),
                    TokenKind::WHILE => return self.parse_while_stmt(),
                    TokenKind::FOR => return self.parse_for_stmt(),
                    TokenKind::CLASS => return self.parse_class(),
                    TokenKind::FN => return self.parse_function(),
                    TokenKind::RETURN => return self.parse_return(),
                    _ => self.parse_expr_stmt(),
                }?;
                self.advance_or_err(TokenKind::SEMICOLON, ErrorMsg::MissingSemicolon)?;
                Ok(item)
            }
            None => return Err(format!("Parse error: {}", ErrorMsg::EndOfStream)),
        }
    }

    fn parse_let_stmt(&mut self) -> Result<Item, Error> {
        // Consume the `let` keyword
        self.advance();
        let name = self.advance_or_err(TokenKind::IDENT, ErrorMsg::InvalidIdent)?;
        let init = if self.advance_if(|t| t.kind == TokenKind::EQUAL).is_some() {
            Some(self.parse_expr()?)
        } else {
            None
        };

        Ok(Item::LetStmt {
            name: Literal::Ident(name.lexeme.clone()),
            init,
        })
    }

    fn parse_if_stmt(&mut self) -> Result<Item, Error> {
        // Consume the `if` keyword
        self.advance();
        self.advance_or_err(TokenKind::LPAREN, ErrorMsg::MissingOpeningParen)?;
        let condition = self.parse_expr()?;
        self.advance_or_err(TokenKind::RPAREN, ErrorMsg::MissingClosingParen)?;
        let if_item = self.parse_item()?;
        let else_item = if self.advance_if(|t| t.kind == TokenKind::ELSE).is_some() {
            Some(Box::new(self.parse_item()?))
        } else {
            None
        };

        Ok(Item::IfStmt {
            condition,
            if_item: Box::new(if_item),
            else_item,
        })
    }

    fn parse_while_stmt(&mut self) -> Result<Item, Error> {
        // Consume the `while` keyword
        self.advance();
        self.advance_or_err(TokenKind::LPAREN, ErrorMsg::MissingOpeningParen)?;
        let condition = self.parse_expr()?;
        self.advance_or_err(TokenKind::RPAREN, ErrorMsg::MissingClosingParen)?;

        Ok(Item::WhileStmt {
            condition,
            body: Box::new(self.parse_item()?),
        })
    }

    fn parse_for_stmt(&mut self) -> Result<Item, Error> {
        // Consume the `for` keyword
        self.advance();
        self.advance_or_err(TokenKind::LPAREN, ErrorMsg::MissingOpeningParen)?;
        let init = match self.stream.peek() {
            Some(&t) => match t.kind {
                TokenKind::SEMICOLON => None,
                TokenKind::LET => Some(self.parse_let_stmt()?),
                _ => Some(self.parse_expr_stmt()?),
            },
            None => return Err(format!("Parse error: {}", ErrorMsg::EndOfStream)),
        };
        self.advance_or_err(TokenKind::SEMICOLON, ErrorMsg::MissingSemicolon)?;

        let condition = if self
            .stream
            .peek()
            .filter(|t| t.kind == TokenKind::SEMICOLON)
            .is_some()
        {
            Expr::Literal(Literal::Boolean(true))
        } else {
            self.parse_expr()?
        };
        self.advance_or_err(TokenKind::SEMICOLON, ErrorMsg::MissingSemicolon)?;

        let modifier = if self
            .stream
            .peek()
            .filter(|t| t.kind == TokenKind::RPAREN)
            .is_some()
        {
            None
        } else {
            Some(self.parse_expr()?)
        };
        self.advance_or_err(TokenKind::RPAREN, ErrorMsg::MissingClosingParen)?;

        let mut body = self.parse_item()?;
        // If the modifier is present, create
        // a block and place it at the end
        if let Some(m) = modifier {
            body = Item::Block(vec![body, Item::ExprStmt(m)]);
        }
        // Create a while loop with the condition and body
        body = Item::WhileStmt {
            condition,
            body: Box::new(body),
        };
        // If the initialiser is present, create
        // a block and place it at the beginning,
        // followed by the actual while loop block
        if let Some(i) = init {
            body = Item::Block(vec![i, body]);
        }

        Ok(body)
    }

    fn parse_class(&mut self) -> Result<Item, Error> {
        // Consume the `class` keyword
        self.advance();
        let name = Literal::Ident(
            self.advance_or_err(TokenKind::IDENT, ErrorMsg::InvalidIdent)?
                .lexeme,
        );
        self.advance_or_err(TokenKind::LBRACE, ErrorMsg::MissingOpeningBrace)?;
        let mut methods = vec![];
        while self
            .stream
            .peek()
            .filter(|t| t.kind == TokenKind::RBRACE)
            .is_none()
        {
            methods.push(self.parse_function()?);
        }
        self.advance_or_err(TokenKind::RBRACE, ErrorMsg::MissingClosingBrace)?;

        Ok(Item::Class { name, methods })
    }

    fn parse_function(&mut self) -> Result<Item, Error> {
        // Consume the `fn` keyword
        self.advance();
        let name = Literal::Ident(
            self.advance_or_err(TokenKind::IDENT, ErrorMsg::InvalidIdent)?
                .lexeme,
        );
        self.advance_or_err(TokenKind::LPAREN, ErrorMsg::MissingOpeningParen)?;
        let mut args = vec![];
        while self
            .stream
            .peek()
            .filter(|t| t.kind == TokenKind::RPAREN)
            .is_none()
        {
            args.push(Literal::Ident(
                self.advance_or_err(TokenKind::IDENT, ErrorMsg::InvalidIdent)?
                    .lexeme,
            ));
            if self.advance_if(|t| t.kind == TokenKind::COMMA).is_none() {
                break;
            }
        }
        self.advance_or_err(TokenKind::RPAREN, ErrorMsg::MissingClosingParen)?;
        if let Some(&t) = self.stream.peek() {
            if t.kind != TokenKind::LBRACE {
                return Err(Self::error(t, ErrorMsg::MissingOpeningBrace));
            }
        }
        let Item::Block(body) = self.parse_block()? else {
            unreachable!("parsing a block must return a body")
        };

        Ok(Item::Function { name, args, body })
    }

    fn parse_return(&mut self) -> Result<Item, Error> {
        // Consume the `return` keyword
        self.advance();
        if self
            .advance_if(|t| t.kind == TokenKind::SEMICOLON)
            .is_some()
        {
            return Ok(Item::ReturnStmt(Expr::Literal(Literal::Null)));
        }
        let value = self.parse_expr()?;
        self.advance_or_err(TokenKind::SEMICOLON, ErrorMsg::MissingSemicolon)?;

        Ok(Item::ReturnStmt(value))
    }

    fn parse_expr_stmt(&mut self) -> Result<Item, Error> {
        Ok(Item::ExprStmt(self.parse_expr()?))
    }

    fn parse_block(&mut self) -> Result<Item, Error> {
        let mut items = Vec::default();
        // Consume the opening brace
        self.advance();
        while let Some(&t) = self.stream.peek() {
            if matches!(t.kind, TokenKind::RBRACE) {
                break;
            }
            items.push(self.parse_item()?);
        }

        match self.stream.peek() {
            Some(&t) => {
                if t.kind == TokenKind::RBRACE {
                    // Consume the closing brace
                    self.advance();
                    Ok(Item::Block(items))
                } else {
                    Err(Self::error(t, ErrorMsg::MissingClosingBrace))
                }
            }
            None => Err(Self::eof_error(ErrorMsg::MissingClosingBrace)),
        }
    }

    fn parse_expr(&mut self) -> Result<Expr, Error> {
        self.parse_assignment()
    }

    fn parse_assignment(&mut self) -> Result<Expr, Error> {
        let lhs = self.parse_logical_or()?;
        let Some(eq) = (if self
            .stream
            .peek()
            .filter(|&&t| t.kind == TokenKind::EQUAL)
            .is_some()
        {
            self.stream.next()
        } else {
            None
        }) else {
            return Ok(lhs);
        };
        let rhs = self.parse_assignment()?;
        match lhs {
            Expr::Literal(lit @ Literal::Ident(_)) => Ok(Expr::Assignment {
                name: lit,
                value: Box::new(rhs),
            }),
            Expr::FieldGet { object, field } => {
                if matches!(field, Literal::Ident(_)) {
                    Ok(Expr::FieldSet {
                        object,
                        field,
                        value: Box::new(rhs),
                    })
                } else {
                    // The error is manually generated as there is
                    // no single token for the lvalue identifier
                    Err(format!(
                        "Parse error at line {}: {}",
                        eq.line + 1,
                        ErrorMsg::InvalidField,
                    ))
                }
            }
            _ => {
                // The error is manually generated as there is
                // no single token for the lvalue identifier
                Err(format!(
                    "Parse error at line {}: {}",
                    eq.line + 1,
                    ErrorMsg::InvalidAssignment,
                ))
            }
        }
    }

    fn parse_logical_or(&mut self) -> Result<Expr, Error> {
        let mut lhs = self.parse_logical_and()?;
        while self.advance_if(|t| t.kind == TokenKind::OR).is_some() {
            let rhs = self.parse_logical_and()?;
            lhs = Expr::Logical {
                lhs: Box::new(lhs),
                op: ast::LogicalOp::Or,
                rhs: Box::new(rhs),
            };
        }
        Ok(lhs)
    }

    fn parse_logical_and(&mut self) -> Result<Expr, Error> {
        let mut lhs = self.parse_eq()?;
        while self.advance_if(|t| t.kind == TokenKind::AND).is_some() {
            let rhs = self.parse_eq()?;
            lhs = Expr::Logical {
                lhs: Box::new(lhs),
                op: ast::LogicalOp::And,
                rhs: Box::new(rhs),
            };
        }
        Ok(lhs)
    }

    fn parse_eq(&mut self) -> Result<Expr, Error> {
        let mut lhs = self.parse_cmp()?;
        while let Some(op) =
            self.advance_if(|t| matches!(t.kind, TokenKind::EQUAL_EQUAL | TokenKind::BANG_EQUAL))
        {
            // Infallible unwrap as we are ensuring the right token kind above
            let bin_op = ast::BinOp::from_token(op.kind)
                .expect("non-binary operators cannot be present here");
            let rhs = self.parse_cmp()?;
            lhs = Expr::Binary {
                lhs: Box::new(lhs),
                op: bin_op,
                rhs: Box::new(rhs),
            };
        }
        Ok(lhs)
    }

    fn parse_cmp(&mut self) -> Result<Expr, Error> {
        let mut lhs = self.parse_term()?;
        while let Some(op) = self.advance_if(|t| {
            matches!(
                t.kind,
                TokenKind::GREATER
                    | TokenKind::GREATER_EQUAL
                    | TokenKind::LESS
                    | TokenKind::LESS_EQUAL
            )
        }) {
            // Infallible unwrap as we are ensuring the right token kind above
            let bin_op = ast::BinOp::from_token(op.kind)
                .expect("non-binary operators cannot be present here");
            let rhs = self.parse_term()?;
            lhs = Expr::Binary {
                lhs: Box::new(lhs),
                op: bin_op,
                rhs: Box::new(rhs),
            };
        }
        Ok(lhs)
    }

    fn parse_term(&mut self) -> Result<Expr, Error> {
        let mut lhs = self.parse_factor()?;
        while let Some(op) = self.advance_if(|t| {
            matches!(
                t.kind,
                TokenKind::PLUS | TokenKind::MINUS | TokenKind::MODULO
            )
        }) {
            // Infallible unwrap as we are ensuring the right token kind above
            let bin_op = ast::BinOp::from_token(op.kind)
                .expect("non-binary operators cannot be present here");
            let rhs = self.parse_factor()?;
            lhs = Expr::Binary {
                lhs: Box::new(lhs),
                op: bin_op,
                rhs: Box::new(rhs),
            };
        }
        Ok(lhs)
    }

    fn parse_factor(&mut self) -> Result<Expr, Error> {
        let mut lhs = self.parse_unary()?;
        while let Some(op) =
            self.advance_if(|t| matches!(t.kind, TokenKind::SLASH | TokenKind::STAR))
        {
            // Infallible unwrap as we are ensuring the right token kind above
            let bin_op = ast::BinOp::from_token(op.kind)
                .expect("non-binary operators cannot be present here");
            let rhs = self.parse_unary()?;
            lhs = Expr::Binary {
                lhs: Box::new(lhs),
                op: bin_op,
                rhs: Box::new(rhs),
            };
        }
        Ok(lhs)
    }

    fn parse_unary(&mut self) -> Result<Expr, Error> {
        let expr = if let Some(op) = self.advance_if(|t| {
            matches!(
                t.kind,
                TokenKind::BANG | TokenKind::MINUS | TokenKind::INCREMENT | TokenKind::DECREMENT
            )
        }) {
            Expr::Unary {
                // Infallible unwrap as we are ensuring the right token kind above
                op: ast::UnaryOp::from_token(op.kind)
                    .expect("non-unary operators cannot be present here"),
                expr: Box::new(self.parse_unary()?),
            }
        } else {
            self.parse_func_call()?
        };

        Ok(expr)
    }

    fn parse_func_call(&mut self) -> Result<Expr, Error> {
        let mut expr = self.parse_primary()?;
        loop {
            if let Some(t) = self.stream.peek() {
                match t.kind {
                    TokenKind::LPAREN => {
                        // Consume the opening parenthesis
                        self.advance();
                        let mut args = vec![];
                        if self.advance_if(|t| t.kind == TokenKind::RPAREN).is_none() {
                            loop {
                                args.push(self.parse_expr()?);
                                if self.advance_if(|t| t.kind == TokenKind::COMMA).is_none() {
                                    break;
                                }
                            }
                            // Consume the closing parenthesis
                            self.advance_or_err(TokenKind::RPAREN, ErrorMsg::MissingClosingParen)?;
                        }
                        expr = Expr::Call {
                            func: Box::new(expr),
                            args,
                        };
                    }
                    TokenKind::DOT => {
                        // Consume the dot
                        self.advance();
                        expr = Expr::FieldGet {
                            object: Box::new(expr),
                            field: Literal::Ident(
                                self.advance_or_err(TokenKind::IDENT, ErrorMsg::InvalidIdent)?
                                    .lexeme,
                            ),
                        }
                    }
                    _ => break,
                }
            }
        }

        Ok(expr)
    }

    fn parse_primary(&mut self) -> Result<Expr, Error> {
        let lit = match self.advance() {
            Some(t) => match t.kind {
                TokenKind::TRUE => Literal::Boolean(true),
                TokenKind::FALSE => Literal::Boolean(false),
                TokenKind::NULL => Literal::Null,
                TokenKind::NUMBER => {
                    Literal::Number(t.lexeme.parse().expect("failed to parse number"))
                }
                TokenKind::STRING => Literal::Str(t.lexeme.clone()),
                TokenKind::LPAREN => return self.parse_group(),
                TokenKind::IDENT | TokenKind::THIS => Literal::Ident(t.lexeme.clone()),
                _ => return Err(Self::error(t, ErrorMsg::UnexpectedToken)),
            },
            None => return Err(format!("Parse error: {}", ErrorMsg::EndOfStream)),
        };
        Ok(Expr::Literal(lit))
    }

    fn parse_group(&mut self) -> Result<Expr, Error> {
        let expr = self.parse_expr()?;
        if let Some(t) = self.stream.peek() {
            if t.kind == TokenKind::RPAREN {
                self.advance();
                return Ok(Expr::Group(Box::new(expr)));
            } else {
                Err(Self::error(t, ErrorMsg::MissingClosingParen))
            }
        } else {
            Err(Self::eof_error(ErrorMsg::MissingClosingParen))
        }
    }

    fn advance(&mut self) -> Option<&Token> {
        self.stream.next()
    }

    fn advance_if<F>(&mut self, cond: F) -> Option<&Token>
    where
        F: FnOnce(&Token) -> bool,
    {
        if self.stream.peek().filter(|&&t| cond(t)).is_some() {
            self.advance()
        } else {
            None
        }
    }

    fn advance_or_err(&mut self, kind: TokenKind, msg: ErrorMsg) -> Result<Token, Error> {
        if let Some(&t) = self.stream.peek() {
            if t.kind == kind {
                self.advance();
                Ok(t.clone())
            } else {
                Err(Self::error(t, msg))
            }
        } else {
            Err(Self::eof_error(msg))
        }
    }

    fn sync(&mut self) {
        if self
            .advance_if(|t| t.kind == TokenKind::SEMICOLON)
            .is_some()
        {
            return;
        }
        while self
            .stream
            .peek()
            .filter(|t| {
                !matches!(
                    t.kind,
                    TokenKind::CLASS
                        | TokenKind::FN
                        | TokenKind::LET
                        | TokenKind::FOR
                        | TokenKind::IF
                        | TokenKind::WHILE
                        | TokenKind::RETURN
                )
            })
            .is_some()
        {
            self.advance();
        }
    }

    fn error(token: &Token, msg: ErrorMsg) -> Error {
        format!("Parse error at line {}: {} {}", token.line + 1, msg, token)
    }

    fn eof_error(msg: ErrorMsg) -> Error {
        format!("Parse error: {} {}", msg, ErrorMsg::EndOfStream)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::lex::Lexer;

    fn parse_test(input: &str, expected: Source) {
        let tokens = Lexer::new(input).lex_all_sanitised().unwrap();
        let source = Parser::new(&tokens).parse_all().unwrap();
        assert_eq!(source, expected);
    }

    fn parse_err_test(input: &str, expected: &str) {
        let tokens = Lexer::new(input).lex_all_sanitised().unwrap();
        let err = Parser::new(&tokens)
            .parse_all()
            .map_err(|v| v.first().unwrap().to_owned())
            .unwrap_err();
        assert_eq!(err, expected);
    }

    #[test]
    fn let_stmt() {
        parse_test(
            "let x = 42;",
            Source {
                items: vec![Item::LetStmt {
                    name: Literal::Ident("x".to_owned()),
                    init: Some(Expr::Literal(Literal::Number(42.0))),
                }],
            },
        );
    }

    #[test]
    fn expr_stmt() {
        parse_test(
            "2.0;",
            Source {
                items: vec![Item::ExprStmt(Expr::Literal(Literal::Number(2.0)))],
            },
        );
    }

    #[test]
    fn if_stmt() {
        parse_test(
            "if (true) { 1; } else { 0; }",
            Source {
                items: vec![Item::IfStmt {
                    condition: Expr::Literal(Literal::Boolean(true)),
                    if_item: Box::new(Item::Block(vec![Item::ExprStmt(Expr::Literal(
                        Literal::Number(1.0),
                    ))])),
                    else_item: Some(Box::new(Item::Block(vec![Item::ExprStmt(Expr::Literal(
                        Literal::Number(0.0),
                    ))]))),
                }],
            },
        );
    }

    #[test]
    fn while_stmt() {
        parse_test(
            "while (true) { 1; }",
            Source {
                items: vec![Item::WhileStmt {
                    condition: Expr::Literal(Literal::Boolean(true)),
                    body: Box::new(Item::Block(vec![Item::ExprStmt(Expr::Literal(
                        Literal::Number(1.0),
                    ))])),
                }],
            },
        );
    }

    #[test]
    fn for_stmt() {
        let var = "a".to_string();
        parse_test(
            "for (let a = 0; a < 5; a = a + 1) { a; }",
            Source {
                items: vec![Item::Block(vec![
                    Item::LetStmt {
                        name: Literal::Ident(var.clone()),
                        init: Some(Expr::Literal(Literal::Number(0.0))),
                    },
                    Item::WhileStmt {
                        condition: Expr::Binary {
                            lhs: Box::new(Expr::Literal(Literal::Ident(var.clone()))),
                            op: ast::BinOp::Less,
                            rhs: Box::new(Expr::Literal(Literal::Number(5.0))),
                        },
                        body: Box::new(Item::Block(vec![
                            Item::Block(vec![Item::ExprStmt(Expr::Literal(Literal::Ident(
                                var.clone(),
                            )))]),
                            Item::ExprStmt(Expr::Assignment {
                                name: Literal::Ident(var.clone()),
                                value: Box::new(Expr::Binary {
                                    lhs: Box::new(Expr::Literal(Literal::Ident(var.clone()))),
                                    op: ast::BinOp::Plus,
                                    rhs: Box::new(Expr::Literal(Literal::Number(1.0))),
                                }),
                            }),
                        ])),
                    },
                ])],
            },
        );
    }

    #[test]
    fn block() {
        parse_test(
            "{ let x = 1; let y = 2; }",
            Source {
                items: vec![Item::Block(vec![
                    Item::LetStmt {
                        name: Literal::Ident("x".to_owned()),
                        init: Some(Expr::Literal(Literal::Number(1.0))),
                    },
                    Item::LetStmt {
                        name: Literal::Ident("y".to_owned()),
                        init: Some(Expr::Literal(Literal::Number(2.0))),
                    },
                ])],
            },
        );
    }

    #[test]
    fn logical() {
        parse_test(
            "1 and 2 or 3;",
            Source {
                items: vec![Item::ExprStmt(Expr::Logical {
                    lhs: Box::new(Expr::Logical {
                        lhs: Box::new(Expr::Literal(Literal::Number(1.0))),
                        op: ast::LogicalOp::And,
                        rhs: Box::new(Expr::Literal(Literal::Number(2.0))),
                    }),
                    op: ast::LogicalOp::Or,
                    rhs: Box::new(Expr::Literal(Literal::Number(3.0))),
                })],
            },
        );
    }

    #[test]
    fn missing_semicolon() {
        parse_err_test(
            "let x = 1",
            format!("Parse error: {} end of stream", ErrorMsg::MissingSemicolon).as_str(),
        );
    }

    #[test]
    fn missing_closing_paren() {
        parse_err_test(
            "(1 + 2 * 3;",
            format!("Parse error at line 1: {} ;", ErrorMsg::MissingClosingParen).as_str(),
        );
    }

    #[test]
    fn missing_closing_brace() {
        parse_err_test(
            "{ a;",
            format!(
                "Parse error: {} end of stream",
                ErrorMsg::MissingClosingBrace
            )
            .as_str(),
        );
    }
}
