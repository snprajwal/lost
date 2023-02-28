use crate::{
    environment::{Env, Type},
    error::{make, Error, ErrorMsg},
};
use lost_syntax::ast::{BinOp, Expr, Item, Literal, UnaryOp};

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

    fn interpret_block(&mut self, items: Vec<Item>) -> Result<(), Error> {
        let old_env = self.env.clone();
        // Create a new env with the current env as the parent
        let new_env = Env::with_parent(self.env.clone());
        self.env = new_env;
        let res = self.interpret_all(items);
        self.env = old_env;
        res
    }

    fn interpret_expr(&mut self, expr: Expr) -> Result<Type, Error> {
        match expr {
            Expr::Assignment { name, value } => self.interpret_assignment(name, *value),
            Expr::Literal(l) => Ok(self.env.from_literal(l)?),
            Expr::Unary { op, expr } => self.interpret_unary(op, *expr),
            Expr::Binary { lhs, op, rhs } => self.interpret_binary(*lhs, op, *rhs),
            Expr::Group(e) => self.interpret_expr(*e),
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
        self.to_bool(&left) == self.to_bool(&right)
    }
}
