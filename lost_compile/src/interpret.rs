use lost_syntax::ast::{AstNode, BinOp, Expr, Literal, UnaryOp};
use std::fmt::Display;

pub type Error = String;

#[derive(Debug)]
pub enum ErrorMsg {
    ExpectedNumber,
    ExpectedNumOrStr,
    InvalidStrOp,
    MismatchedTypes,
}

impl Display for ErrorMsg {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(match self {
            Self::ExpectedNumber => "expected numeric operand",
            Self::ExpectedNumOrStr => "expected both operands to be numeric or string",
            Self::InvalidStrOp => "invalid operation on strings",
            Self::MismatchedTypes => "mismatched types of operands",
        })
    }
}

pub struct Interpreter {
    root: AstNode,
}

impl Interpreter {
    pub fn new(root: AstNode) -> Self {
        Self { root }
    }

    pub fn interpret(&self) -> Result<String, Error> {
        self.interpret_expr(self.root.expr.clone())
            .map(|l| l.to_string())
    }

    fn interpret_expr(&self, expr: Expr) -> Result<Literal, Error> {
        match expr {
            Expr::Literal(l) => Ok(l),
            Expr::Unary { op, expr } => self.interpret_unary(op, *expr),
            Expr::Binary { lhs, op, rhs } => self.interpret_binary(*lhs, op, *rhs),
            Expr::Group(e) => self.interpret_expr(*e),
        }
    }

    fn interpret_unary(&self, op: UnaryOp, expr: Expr) -> Result<Literal, Error> {
        let lit = self.interpret_expr(expr)?;
        match op {
            UnaryOp::Minus => {
                if let Literal::Number(n) = lit {
                    Ok(Literal::Number(-n))
                } else {
                    Err(Self::error(ErrorMsg::ExpectedNumber, lit.to_string()))
                }
            }
            UnaryOp::Bang => return Ok(Literal::Boolean(!self.to_bool(&lit))),
        }
    }

    fn to_bool(&self, lit: &Literal) -> bool {
        match lit {
            Literal::Null => false,
            Literal::Boolean(b) => *b,
            _ => true,
        }
    }

    fn interpret_binary(&self, lhs: Expr, op: BinOp, rhs: Expr) -> Result<Literal, Error> {
        let left = self.interpret_expr(lhs)?;
        let right = self.interpret_expr(rhs)?;

        // Handle string concatenation
        if let Literal::Str(left_str) = left {
            if op == BinOp::Plus {
                if let Literal::Str(right_str) = right {
                    return Ok(Literal::Str(left_str + &right_str));
                } else {
                    return Err(Self::error(ErrorMsg::ExpectedNumOrStr, right.to_string()));
                }
            } else {
                return Err(Self::error(ErrorMsg::InvalidStrOp, op.to_string()));
            }
        };

        // Handle == and !=
        if matches!(op, BinOp::EqualEqual | BinOp::BangEqual) {
            let Some(is_eq) = Self::is_eq(&left, &right) else { return Err(Self::error(ErrorMsg::MismatchedTypes, right.to_string()))};
            return Ok(Literal::Boolean(if op == BinOp::BangEqual {
                !is_eq
            } else {
                is_eq
            }));
        }

        let Literal::Number(left_num) = left else { return Err(Self::error(ErrorMsg::ExpectedNumber, left.to_string()))};
        let Literal::Number(right_num) = right else { return Err(Self::error(ErrorMsg::ExpectedNumber, right.to_string()))};

        Ok(match op {
            BinOp::Plus => Literal::Number(left_num + right_num),
            BinOp::Minus => Literal::Number(left_num - right_num),
            BinOp::Star => Literal::Number(left_num * right_num),
            BinOp::Slash => Literal::Number(left_num / right_num),
            BinOp::Greater => Literal::Boolean(left_num > right_num),
            BinOp::GreaterEqual => Literal::Boolean(left_num >= right_num),
            BinOp::Less => Literal::Boolean(left_num < right_num),
            BinOp::LessEqual => Literal::Boolean(left_num <= right_num),
            _ => unreachable!(),
        })
    }

    fn is_eq(left: &Literal, right: &Literal) -> Option<bool> {
        Some(left == right)
        // match (left, right) {
        //     (Literal::Number(_), Literal::Number(_))
        //     | (Literal::Boolean(_), Literal::Boolean(_))
        //     | (Literal::Str(_), Literal::Str(_))
        //     | (Literal::Null, Literal::Null) => Some(left == right),
        //     _ => None,
        // }
    }

    fn error(msg: ErrorMsg, val: String) -> Error {
        format!("{}, found {}", msg, val)
    }
}
