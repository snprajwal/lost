use crate::{
    environment::{Env, Type},
    error::{make, Error, ErrorMsg},
};
use lost_syntax::ast::{BinOp, Expr, Item, Literal, LogicalOp, UnaryOp};

#[derive(Default, Debug)]
pub struct Interpreter {
    pub env: Env,
}

impl Interpreter {
    pub fn new(env: Option<Env>) -> Self {
        Self {
            env: env.unwrap_or_default(),
        }
    }

    pub fn interpret_all(&mut self, items: Vec<Item>) -> Result<(), Error> {
        let mut item_iter = items.into_iter();
        while let Some(item) = item_iter.next() {
            self.interpret(item)?;
        }
        Ok(())
    }

    pub fn interpret(&mut self, item: Item) -> Result<(), Error> {
        self.interpret_item(item)
    }

    fn interpret_item(&mut self, item: Item) -> Result<(), Error> {
        self.interpret_stmt(item)
    }

    // This is a hack to allow PrintStmt for debugging,
    // and will be removed once functions are introduced
    fn interpret_stmt(&mut self, stmt: Item) -> Result<(), Error> {
        match stmt {
            Item::PrintStmt(expr) => println!("{}", self.interpret_expr(expr)?),
            Item::ExprStmt(expr) => {
                self.interpret_expr(expr)?;
            }
            Item::LetStmt { name, init } => return self.interpret_let_stmt(name, init),
            Item::IfStmt {
                condition,
                if_item,
                else_item,
            } => return self.interpret_if_stmt(condition, *if_item, else_item.map(|item| *item)),
            Item::WhileStmt { condition, body } => {
                return self.interpret_while_stmt(condition, *body)
            }
            Item::Block(items) => return self.interpret_block(items),
        };
        Ok(())
    }

    fn interpret_let_stmt(&mut self, name: Literal, init: Option<Expr>) -> Result<(), Error> {
        let Literal::Ident(ident) = name else { unreachable!("Non-identifiers cannot be passed to this function") };
        let value = match init {
            Some(expr) => self.interpret_expr(expr)?,
            None => Type::Null,
        };
        self.env.set(ident, value);
        Ok(())
    }

    fn interpret_if_stmt(
        &mut self,
        condition: Expr,
        if_item: Item,
        else_item: Option<Item>,
    ) -> Result<(), Error> {
        if self.interpret_expr(condition).map(|t| self.to_bool(&t))? {
            self.interpret_item(if_item)?;
        } else {
            if let Some(item) = else_item {
                self.interpret_item(item)?;
            }
        }

        Ok(())
    }

    fn interpret_while_stmt(&mut self, condition: Expr, body: Item) -> Result<(), Error> {
        while self
            .interpret_expr(condition.clone())
            .map(|t| self.to_bool(&t))?
        {
            self.interpret_item(body.clone())?;
        }

        Ok(())
    }

    fn interpret_block(&mut self, items: Vec<Item>) -> Result<(), Error> {
        // Create a new env with the current env as the parent
        // and set it as the active env for the block scope
        self.env = Env::with_parent(self.env.clone());
        let res = self.interpret_all(items);
        self.env = *self
            .env
            .clone()
            .parent
            .expect("Parent env must be present for block envs");
        res
    }

    fn interpret_expr(&mut self, expr: Expr) -> Result<Type, Error> {
        match expr {
            Expr::Assignment { name, value } => self.interpret_assignment(name, *value),
            Expr::Literal(l) => Ok(self.env.from_literal(l)?),
            Expr::Unary { op, expr } => self.interpret_unary(op, *expr),
            Expr::Binary { lhs, op, rhs } => self.interpret_binary(*lhs, op, *rhs),
            Expr::Group(e) => self.interpret_expr(*e),
            Expr::Logical { lhs, op, rhs } => self.interpret_logical(*lhs, op, *rhs),
        }
    }

    fn interpret_assignment(&mut self, name: Literal, expr: Expr) -> Result<Type, Error> {
        let value = self.interpret_expr(expr)?;
        self.env.assign(name.to_string(), value)
    }

    fn interpret_unary(&mut self, op: UnaryOp, expr: Expr) -> Result<Type, Error> {
        let lit = self.interpret_expr(expr)?;
        match op {
            UnaryOp::Minus => {
                if let Type::Number(n) = lit {
                    Ok(Type::Number(-n))
                } else {
                    Err(make(ErrorMsg::ExpectedNumber, lit.to_string()))
                }
            }
            UnaryOp::Bang => return Ok(Type::Boolean(!self.to_bool(&lit))),
        }
    }

    fn to_bool(&self, lit: &Type) -> bool {
        match lit {
            Type::Null => false,
            Type::Boolean(b) => *b,
            _ => true,
        }
    }

    fn interpret_logical(&mut self, lhs: Expr, op: LogicalOp, rhs: Expr) -> Result<Type, Error> {
        let left = self.interpret_expr(lhs)?;
        return match (op, self.to_bool(&left)) {
            (LogicalOp::Or, true) | (LogicalOp::And, false) => Ok(left),
            _ => self.interpret_expr(rhs),
        };
    }

    fn interpret_binary(&mut self, lhs: Expr, op: BinOp, rhs: Expr) -> Result<Type, Error> {
        let left = self.interpret_expr(lhs)?;
        let right = self.interpret_expr(rhs)?;

        // Handle string concatenation
        if let Type::Str(left_str) = left {
            if op == BinOp::Plus {
                if let Type::Str(right_str) = right {
                    return Ok(Type::Str(left_str + &right_str));
                }
                return Err(make(ErrorMsg::ExpectedNumOrStr, right.to_string()));
            }
            return Err(make(ErrorMsg::InvalidStrOp, op.to_string()));
        };

        // Handle == and !=
        if op == BinOp::EqualEqual {
            return Ok(Type::Boolean(self.is_eq(&left, &right)));
        }
        if op == BinOp::BangEqual {
            return Ok(Type::Boolean(!(self.is_eq(&left, &right))));
        }

        // All other comparisons can be performed only on numbers.
        // It might be possible to perform them across types, but
        // that gives the user a gun to shoot their foot with, and
        // is hence not implemented. No numero, no bueno ;)
        let Type::Number(left_num) = left else { return Err(make(ErrorMsg::ExpectedNumber, left.to_string()))};
        let Type::Number(right_num) = right else { return Err(make(ErrorMsg::ExpectedNumber, right.to_string()))};

        Ok(match op {
            BinOp::Plus => Type::Number(left_num + right_num),
            BinOp::Minus => Type::Number(left_num - right_num),
            BinOp::Star => Type::Number(left_num * right_num),
            BinOp::Slash => Type::Number(left_num / right_num),
            BinOp::Greater => Type::Boolean(left_num > right_num),
            BinOp::GreaterEqual => Type::Boolean(left_num >= right_num),
            BinOp::Less => Type::Boolean(left_num < right_num),
            BinOp::LessEqual => Type::Boolean(left_num <= right_num),
            _ => unreachable!("Non-binary operators cannot be stored in this variable"),
        })
    }

    fn is_eq(&self, left: &Type, right: &Type) -> bool {
        match (left, right) {
            (Type::Number(m), Type::Number(n)) => m == n,
            (Type::Str(m), Type::Str(n)) => m == n,
            _ => self.to_bool(&left) == self.to_bool(&right),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn let_stmt() {
        let mut interpreter = Interpreter::new(None);
        let var = "x".to_string();
        let name = Literal::Ident(var.clone());
        let init = Some(Expr::Literal(Literal::Number(5.0)));
        assert!(interpreter.interpret_let_stmt(name, init).is_ok());
        assert_eq!(interpreter.env.get(var).unwrap(), Type::Number(5.0));
    }

    #[test]
    fn if_stmt() {
        let mut interpreter = Interpreter::new(None);
        let condition = Expr::Literal(Literal::Boolean(true));
        let if_item = Item::ExprStmt(Expr::Literal(Literal::Str("hello".to_string())));
        let else_item = Some(Item::ExprStmt(Expr::Literal(Literal::Str(
            "world".to_string(),
        ))));
        assert!(interpreter
            .interpret_if_stmt(condition, if_item, else_item)
            .is_ok());
    }

    #[test]
    fn while_stmt() {
        let mut interpreter = Interpreter::new(None);
        let condition = Expr::Literal(Literal::Boolean(false));
        let body = Item::ExprStmt(Expr::Literal(Literal::Str("hello".to_string())));
        assert!(interpreter.interpret_while_stmt(condition, body).is_ok());
    }

    #[test]
    fn block() {
        let mut interpreter = Interpreter::new(None);
        let items = vec![
            Item::LetStmt {
                name: Literal::Ident("x".to_string()),
                init: Some(Expr::Literal(Literal::Number(5.0))),
            },
            Item::Block(vec![Item::ExprStmt(Expr::Literal(Literal::Ident(
                "x".to_string(),
            )))]),
        ];
        assert!(interpreter.interpret_block(items).is_ok());
    }

    #[test]
    fn expr() {
        let mut interpreter = Interpreter::new(None);

        // Number literal
        let result = interpreter.interpret_expr(Expr::Literal(Literal::Number(10.5)));
        assert_eq!(result.unwrap(), Type::Number(10.5));

        // Boolean literal
        let result = interpreter.interpret_expr(Expr::Literal(Literal::Boolean(true)));
        assert_eq!(result.unwrap(), Type::Boolean(true));

        // String literal
        let result = interpreter.interpret_expr(Expr::Literal(Literal::Str("hello".to_string())));
        assert_eq!(result.unwrap(), Type::Str("hello".to_string()));

        // Identifier
        interpreter.env.set("x".to_string(), Type::Number(5.0));
        let result = interpreter.interpret_expr(Expr::Literal(Literal::Ident("x".to_string())));
        assert_eq!(result.unwrap(), Type::Number(5.0));

        // Unary minus
        let result = interpreter.interpret_expr(Expr::Unary {
            op: UnaryOp::Minus,
            expr: Box::new(Expr::Literal(Literal::Number(10.0))),
        });
        assert_eq!(result.unwrap(), Type::Number(-10.0));

        // Unary bang
        let result = interpreter.interpret_expr(Expr::Unary {
            op: UnaryOp::Bang,
            expr: Box::new(Expr::Literal(Literal::Boolean(false))),
        });
        assert_eq!(result.unwrap(), Type::Boolean(true));

        // Addition
        let result = interpreter.interpret_expr(Expr::Binary {
            lhs: Box::new(Expr::Literal(Literal::Number(10.0))),
            op: BinOp::Plus,
            rhs: Box::new(Expr::Literal(Literal::Number(20.0))),
        });
        assert_eq!(result.unwrap(), Type::Number(30.0));

        // String concatenation
        let result = interpreter.interpret_expr(Expr::Binary {
            lhs: Box::new(Expr::Literal(Literal::Str("hello".to_string()))),
            op: BinOp::Plus,
            rhs: Box::new(Expr::Literal(Literal::Str("world".to_string()))),
        });
        assert_eq!(result.unwrap(), Type::Str("helloworld".to_string()));

        // Equality
        let result = interpreter.interpret_expr(Expr::Binary {
            lhs: Box::new(Expr::Literal(Literal::Number(10.0))),
            op: BinOp::EqualEqual,
            rhs: Box::new(Expr::Literal(Literal::Number(10.0))),
        });
        assert_eq!(result.unwrap(), Type::Boolean(true));

        // Logical and
        let result = interpreter.interpret_expr(Expr::Logical {
            lhs: Box::new(Expr::Literal(Literal::Number(10.0))),
            op: LogicalOp::And,
            rhs: Box::new(Expr::Literal(Literal::Number(20.0))),
        });
        assert_eq!(result.unwrap(), Type::Number(20.0));

        // Logical or
        let result = interpreter.interpret_expr(Expr::Logical {
            lhs: Box::new(Expr::Literal(Literal::Number(10.0))),
            op: LogicalOp::Or,
            rhs: Box::new(Expr::Literal(Literal::Number(20.0))),
        });
        assert_eq!(result.unwrap(), Type::Number(10.0));

        // Inequality
        let result = interpreter.interpret_expr(Expr::Binary {
            lhs: Box::new(Expr::Literal(Literal::Number(10.0))),
            op: BinOp::BangEqual,
            rhs: Box::new(Expr::Literal(Literal::Number(20.0))),
        });
        assert_eq!(result.unwrap(), Type::Boolean(true));
    }
}
