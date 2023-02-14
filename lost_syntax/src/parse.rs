use std::{iter::Peekable, slice::Iter};

use crate::{
    ast::{self, AstNode, Expr, Literal},
    error::{Error, ErrorMsg},
    token::{TextRange, Token, TokenKind},
};

#[derive(Debug)]
pub struct Parser<'a> {
    stream: Peekable<Iter<'a, Token>>,
    current_range: TextRange,
}

impl<'a> Parser<'a> {
    pub fn new(stream: &'a [Token]) -> Self {
        Self {
            stream: stream.iter().peekable(),
            current_range: TextRange { start: 0, end: 0 },
        }
    }

    pub fn parse(mut self) -> Result<AstNode, Error> {
        Ok(AstNode {
            range: self.current_range,
            expr: self.parse_expr()?,
        })
    }

    fn parse_expr(&mut self) -> Result<Expr, Error> {
        self.parse_eq()
    }

    fn parse_eq(&mut self) -> Result<Expr, Error> {
        let mut lhs = self.parse_cmp()?;
        while let Some(op) =
            self.advance_if(|t| matches!(t.kind, TokenKind::EQUAL_EQUAL | TokenKind::BANG_EQUAL))
        {
            // Infallible unwrap as we are ensuring the right token kind above
            let bin_op = ast::BinOp::from_token(op.kind).unwrap();
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
            let bin_op = ast::BinOp::from_token(op.kind).unwrap();
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
        while let Some(op) =
            self.advance_if(|t| matches!(t.kind, TokenKind::PLUS | TokenKind::MINUS))
        {
            // Infallible unwrap as we are ensuring the right token kind above
            let bin_op = ast::BinOp::from_token(op.kind).unwrap();
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
            let bin_op = ast::BinOp::from_token(op.kind).unwrap();
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
        let expr = if let Some(op) =
            self.advance_if(|t| matches!(t.kind, TokenKind::BANG | TokenKind::MINUS))
        {
            Expr::Unary {
                // Infallible unwrap as we are ensuring the right token kind above
                op: ast::UnaryOp::from_token(op.kind).unwrap(),
                expr: Box::new(self.parse_unary()?),
            }
        } else {
            self.parse_primary()?
        };
        Ok(expr)
    }

    fn parse_primary(&mut self) -> Result<Expr, Error> {
        let lit = match self.advance() {
            Some(t) => match t.kind {
                TokenKind::TRUE => Literal::Boolean(true),
                TokenKind::FALSE => Literal::Boolean(false),
                TokenKind::NULL => Literal::Null,
                TokenKind::NUMBER => Literal::Number(t.lexeme.parse().unwrap()),
                TokenKind::STRING => Literal::Str(t.lexeme.clone()),
                TokenKind::LPAREN => return self.parse_group(),
                _ => return Err(Self::error(t, ErrorMsg::UnexpectedToken)),
            },
            None => return Err(ErrorMsg::EndOfStream.to_string()),
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
                Err(Self::error(t, ErrorMsg::MissingParen))
            }
        } else {
            Err(ErrorMsg::EndOfStream.to_string())
        }
    }

    fn advance(&mut self) -> Option<&Token> {
        self.stream.next().and_then(|t| {
            self.current_range += t.range;
            Some(t)
        })
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

    fn error(token: &Token, msg: ErrorMsg) -> Error {
        format!("Error at line {}: {} {}", token.line + 1, msg, token)
    }
}
