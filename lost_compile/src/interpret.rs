use std::{cell::RefCell, cmp::Ordering, collections::HashMap, rc::Rc};

use crate::{
    environment::Env,
    error::{runtime_error, ErrorMsg, Exception},
    types::{Callable, Class, Func, Type},
};
use lost_syntax::ast::{BinOp, Expr, Ident, Item, Literal, LogicalOp, Source, UnaryOp};

#[derive(Default, Debug)]
pub struct Interpreter {
    pub env: Rc<RefCell<Env>>,
    depths: HashMap<Ident, usize>,
}

impl Interpreter {
    pub fn new() -> Self {
        Self {
            env: Env::new(),
            depths: HashMap::default(),
        }
    }

    pub fn interpret(
        &mut self,
        source: Source,
        depths: HashMap<Ident, usize>,
    ) -> Result<(), Exception> {
        self.depths.extend(depths);
        self.interpret_all(&source.items)
    }

    pub fn interpret_all(&mut self, items: &[Item]) -> Result<(), Exception> {
        items
            .iter()
            .try_for_each(|item| self.interpret_item(item.clone()))
    }

    fn interpret_item(&mut self, item: Item) -> Result<(), Exception> {
        match item {
            Item::ExprStmt(expr) => self.interpret_expr(expr).map(|_| ()),
            Item::LetStmt { ident, init } => self.interpret_let_stmt(ident, init),
            Item::IfStmt {
                condition,
                if_item,
                else_item,
            } => self.interpret_if_stmt(condition, *if_item, else_item.map(|item| *item)),
            Item::WhileStmt { condition, body } => self.interpret_while_stmt(condition, *body),
            Item::ReturnStmt(expr) => self.interpret_return_stmt(expr),
            Item::Block(items) => self
                .interpret_block(&items, Env::with_parent(Rc::clone(&self.env)))
                .map(|_| ()),
            Item::Function { ident, args, body } => self.interpret_function(ident, args, body),
            Item::Class {
                ident,
                parent,
                methods,
            } => self.interpret_class(ident, parent, methods),
        }
    }

    fn interpret_let_stmt(&mut self, ident: Ident, init: Option<Expr>) -> Result<(), Exception> {
        let value = match init {
            Some(expr) => self.interpret_expr(expr)?,
            None => Type::Null,
        };
        self.env.borrow_mut().set(ident.name, value);
        Ok(())
    }

    fn interpret_if_stmt(
        &mut self,
        condition: Expr,
        if_item: Item,
        else_item: Option<Item>,
    ) -> Result<(), Exception> {
        if self.interpret_expr(condition).map(|t| self.to_bool(&t))? {
            self.interpret_item(if_item)?;
        } else {
            if let Some(item) = else_item {
                self.interpret_item(item)?;
            }
        }

        Ok(())
    }

    fn interpret_while_stmt(&mut self, condition: Expr, body: Item) -> Result<(), Exception> {
        while self
            .interpret_expr(condition.clone())
            .map(|t| self.to_bool(&t))?
        {
            self.interpret_item(body.clone())?;
        }

        Ok(())
    }

    fn interpret_block(&mut self, items: &[Item], env: Rc<RefCell<Env>>) -> Result<(), Exception> {
        let old_env = Rc::clone(&self.env);
        self.env = env;
        let ret = self.interpret_all(items);
        self.env = old_env;
        ret
    }

    fn interpret_function(
        &mut self,
        ident: Ident,
        args: Vec<Ident>,
        body: Vec<Item>,
    ) -> Result<(), Exception> {
        let func = self.create_function(ident.clone(), args, body);
        self.env.borrow_mut().set(ident.name, Type::Func(func));
        Ok(())
    }

    fn create_function(&self, ident: Ident, args: Vec<Ident>, body: Vec<Item>) -> Func {
        let arg_idents = args.into_iter().map(|arg| arg.name).collect();
        Func {
            name: ident.name,
            args: arg_idents,
            body,
            env: Rc::clone(&self.env),
        }
    }

    fn interpret_class(
        &mut self,
        ident: Ident,
        parent: Option<Ident>,
        methods: Vec<Item>,
    ) -> Result<(), Exception> {
        let (old_env, parent_class) = if let Some(p) = parent {
            let t @ Type::Class(c) = &self.interpret_ident(p.clone())? else {
                return Err(runtime_error(ErrorMsg::ExpectedClass, p.name));
            };
            // Create a new parent env with the parent class
            let old_env = Rc::clone(&self.env);
            self.env = Env::with_parent(Rc::clone(&self.env));
            self.env.borrow_mut().set("super".to_string(), t.clone());
            (Some(old_env), Some(Box::new(c.clone())))
        } else {
            (None, None)
        };
        let mut method_map: HashMap<String, Func> = HashMap::default();
        for method in methods {
            if let Item::Function { ident, args, body } = method {
                let func = self.create_function(ident, args, body);
                method_map.insert(func.name.clone(), func);
            } else {
                unreachable!("non-functions cannot be passed as methods");
            }
        }
        if let Some(env) = old_env {
            // Go back to the old env without `super`
            self.env = env;
        }

        self.env.borrow_mut().set(
            ident.name.clone(),
            Type::Class(Class {
                name: ident.name,
                parent: parent_class,
                methods: method_map,
            }),
        );
        Ok(())
    }

    fn interpret_return_stmt(&mut self, expr: Expr) -> Result<(), Exception> {
        Err(Exception::Return(self.interpret_expr(expr)?))
    }

    fn interpret_expr(&mut self, expr: Expr) -> Result<Type, Exception> {
        match expr {
            Expr::Assignment { name, value } => self.interpret_assignment(name, *value),
            Expr::Literal(lit) => self.interpret_literal(lit),
            Expr::Ident(ident) => self.interpret_ident(ident),
            Expr::Unary { op, expr } => self.interpret_unary(op, *expr),
            Expr::Binary { lhs, op, rhs } => self.interpret_binary(*lhs, op, *rhs),
            Expr::Logical { lhs, op, rhs } => self.interpret_logical(*lhs, op, *rhs),
            Expr::Group(e) => self.interpret_expr(*e),
            Expr::Call { func, args } => self.interpret_call(*func, args),
            Expr::FieldGet { object, field } => self.interpret_field_get(*object, field),
            Expr::FieldSet {
                object,
                field,
                value,
            } => self.interpret_field_set(*object, field, *value),
            Expr::Super(super_, method) => self.interpret_super(super_, method),
        }
    }

    fn interpret_assignment(&mut self, ident: Ident, expr: Expr) -> Result<Type, Exception> {
        let value = self.interpret_expr(expr)?;
        self.env.borrow_mut().assign_at_depth(
            ident.name.clone(),
            value,
            *self
                .depths
                .get(&ident)
                .ok_or_else(|| runtime_error(ErrorMsg::MisresolvedVar, ident.name))?,
        )?;
        Ok(Type::Null)
    }

    fn interpret_literal(&mut self, lit: Literal) -> Result<Type, Exception> {
        Ok(match lit {
            Literal::Str(s) => Type::Str(s),
            Literal::Number(n) => Type::Number(n),
            Literal::Boolean(b) => Type::Boolean(b),
            Literal::Null => Type::Null,
        })
    }

    fn interpret_ident(&mut self, ident: Ident) -> Result<Type, Exception> {
        self.env.borrow().get_at_depth(
            ident.name.clone(),
            *self
                .depths
                .get(&ident)
                .ok_or_else(|| runtime_error(ErrorMsg::MisresolvedVar, ident.name))?,
        )
    }

    fn interpret_unary(&mut self, op: UnaryOp, expr: Expr) -> Result<Type, Exception> {
        let lit = self.interpret_expr(expr.clone())?;
        match op {
            UnaryOp::Minus => {
                if let Type::Number(n) = lit {
                    Ok(Type::Number(-n))
                } else {
                    Err(runtime_error(ErrorMsg::ExpectedNumber, lit.to_string()))
                }
            }
            UnaryOp::Bang => Ok(Type::Boolean(!self.to_bool(&lit))),
            UnaryOp::Increment => {
                let Expr::Ident(ident) = expr else {
                    return Err(runtime_error(ErrorMsg::ExpectedIdent, lit.to_string()));
                };
                if let Type::Number(n) = lit {
                    self.env.borrow_mut().assign_at_depth(
                        ident.name.clone(),
                        Type::Number(n + 1.0),
                        *self
                            .depths
                            .get(&ident)
                            .ok_or_else(|| runtime_error(ErrorMsg::MisresolvedVar, ident.name))?,
                    )?;
                    Ok(Type::Number(n + 1.0))
                } else {
                    Err(runtime_error(ErrorMsg::ExpectedNumber, lit.to_string()))
                }
            }
            UnaryOp::Decrement => {
                let Expr::Ident(ident) = expr else {
                    return Err(runtime_error(ErrorMsg::ExpectedIdent, lit.to_string()));
                };
                if let Type::Number(n) = lit {
                    self.env.borrow_mut().assign_at_depth(
                        ident.name.clone(),
                        Type::Number(n - 1.0),
                        *self
                            .depths
                            .get(&ident)
                            .ok_or_else(|| runtime_error(ErrorMsg::MisresolvedVar, ident.name))?,
                    )?;
                    Ok(Type::Number(n - 1.0))
                } else {
                    Err(runtime_error(ErrorMsg::ExpectedNumber, lit.to_string()))
                }
            }
        }
    }

    fn to_bool(&self, lit: &Type) -> bool {
        match lit {
            Type::Null => false,
            Type::Boolean(b) => *b,
            _ => true,
        }
    }

    fn interpret_logical(
        &mut self,
        lhs: Expr,
        op: LogicalOp,
        rhs: Expr,
    ) -> Result<Type, Exception> {
        let left = self.interpret_expr(lhs)?;
        match (op, self.to_bool(&left)) {
            (LogicalOp::Or, true) | (LogicalOp::And, false) => Ok(left),
            _ => self.interpret_expr(rhs),
        }
    }

    fn interpret_binary(&mut self, lhs: Expr, op: BinOp, rhs: Expr) -> Result<Type, Exception> {
        let left = self.interpret_expr(lhs)?;
        let right = self.interpret_expr(rhs)?;

        // Handle string concatenation
        if let Type::Str(left_str) = left {
            if op == BinOp::Plus {
                if let Type::Str(right_str) = right {
                    return Ok(Type::Str(left_str + &right_str));
                }
                return Err(runtime_error(ErrorMsg::ExpectedNumOrStr, right.to_string()));
            }
            return Err(runtime_error(ErrorMsg::InvalidStrOp, op.to_string()));
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
        let Type::Number(left_num) = left else {
            return Err(runtime_error(ErrorMsg::ExpectedNumber, left.to_string()));
        };
        let Type::Number(right_num) = right else {
            return Err(runtime_error(ErrorMsg::ExpectedNumber, right.to_string()));
        };

        Ok(match op {
            BinOp::Plus => Type::Number(left_num + right_num),
            BinOp::Minus => Type::Number(left_num - right_num),
            BinOp::Star => Type::Number(left_num * right_num),
            BinOp::Modulo => Type::Number(left_num % right_num),
            BinOp::Slash => Type::Number(left_num / right_num),
            BinOp::Greater => Type::Boolean(left_num > right_num),
            BinOp::GreaterEqual => Type::Boolean(left_num >= right_num),
            BinOp::Less => Type::Boolean(left_num < right_num),
            BinOp::LessEqual => Type::Boolean(left_num <= right_num),
            _ => unreachable!("non-binary operators cannot be stored in this variable"),
        })
    }

    fn interpret_call(&mut self, fn_expr: Expr, arg_exprs: Vec<Expr>) -> Result<Type, Exception> {
        let ty = self.interpret_expr(fn_expr)?;
        let func: Box<dyn Callable> = match ty {
            Type::Func(f) => Box::new(f),
            Type::NativeFunc(f) => Box::new(f),
            Type::Class(c) => Box::new(c),
            _ => return Err(runtime_error(ErrorMsg::InvalidCall, ty.to_string())),
        };
        // Ensure the number of arguments matches the function definition
        match func.arity().cmp(&arg_exprs.len()) {
            Ordering::Greater => {
                return Err(runtime_error(
                    ErrorMsg::TooFewArgs,
                    format!("(expected {}, got {})", func.arity(), arg_exprs.len()),
                ))
            }
            Ordering::Less => {
                return Err(runtime_error(
                    ErrorMsg::TooManyArgs,
                    format!("(expected {}, got {})", func.arity(), arg_exprs.len()),
                ))
            }
            _ => (),
        }
        let mut args = vec![];
        for arg in arg_exprs {
            args.push(self.interpret_expr(arg)?);
        }

        func.call(self, &args)
    }

    pub(crate) fn call(&mut self, func: &Func, args: &[Type]) -> Result<Type, Exception> {
        let env = Env::with_parent(Rc::clone(&func.env));
        // Add the arguments to the function env
        for (ident, value) in func.args.iter().zip(args) {
            env.borrow_mut().set(ident.clone(), value.clone());
        }
        // If there is no error or return value, return null
        let Err(exception) = self.interpret_block(&func.body, env) else {
            return Ok(Type::Null);
        };
        // If the exception is a return value, propagate
        // it as an `Ok` value with the return type, else
        // return the error itself
        match exception {
            Exception::Return(val) => Ok(val),
            Exception::Error(_) => Err(exception),
        }
    }

    fn interpret_field_get(&mut self, object: Expr, field: Ident) -> Result<Type, Exception> {
        let ty = self.interpret_expr(object)?;
        let Type::Instance(instance) = ty else {
            return Err(runtime_error(ErrorMsg::ExpectedObject, ty.to_string()));
        };
        instance.get(field.name)
    }

    fn interpret_field_set(
        &mut self,
        object: Expr,
        field: Ident,
        expr: Expr,
    ) -> Result<Type, Exception> {
        let Expr::Ident(ident) = &object else {
            unreachable!("non-identifiers cannot be passed to this function");
        };
        let ty = self.interpret_expr(object.clone())?;
        let Type::Instance(mut instance) = ty else {
            return Err(runtime_error(ErrorMsg::ExpectedObject, ty.to_string()));
        };
        let value = self.interpret_expr(expr)?;
        instance.set(field.name, value)?;
        self.env.borrow_mut().assign_at_depth(
            ident.name.clone(),
            Type::Instance(instance),
            *self
                .depths
                .get(ident)
                .ok_or_else(|| runtime_error(ErrorMsg::MisresolvedVar, ident.name.clone()))?,
        )?;
        Ok(Type::Null)
    }

    fn interpret_super(&self, super_: Ident, method: Ident) -> Result<Type, Exception> {
        let depth = self
            .depths
            .get(&super_)
            .ok_or_else(|| runtime_error(ErrorMsg::MisresolvedVar, super_.name))?;
        let Type::Class(parent) = self
            .env
            .borrow()
            .get_at_depth("super".to_string(), *depth)?
        else {
            unreachable!("`super` cannot be a non-class type")
        };
        let Type::Instance(this) = self
            .env
            .borrow()
            .get_at_depth("this".to_string(), *depth - 1)?
        else {
            unreachable!("`this` cannot be a non-instance type")
        };
        let Some(mut method) = parent.get_method(&method.name) else {
            return Err(runtime_error(ErrorMsg::UndefinedMethod, method.name));
        };
        // Add `this` into a parent env for the function to access
        method.env = Env::with_parent(method.env);
        method
            .env
            .borrow_mut()
            .set("this".to_string(), Type::Instance(this));
        return Ok(Type::Func(method));
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
    use lost_syntax::{ast::Ident, token::TextRange};

    use super::*;

    #[test]
    fn let_stmt() {
        let mut interpreter = Interpreter::new();
        let var = "x".to_string();
        let name = Ident {
            name: var.clone(),
            range: TextRange { start: 0, end: 0 },
        };
        let init = Some(Expr::Literal(Literal::Number(5.0)));
        assert!(interpreter.interpret_let_stmt(name, init).is_ok());
        assert_eq!(
            interpreter.env.borrow().get_at_depth(var, 0).unwrap(),
            Type::Number(5.0)
        );
    }

    #[test]
    fn if_stmt() {
        let mut interpreter = Interpreter::new();
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
        let mut interpreter = Interpreter::new();
        let condition = Expr::Literal(Literal::Boolean(false));
        let body = Item::ExprStmt(Expr::Literal(Literal::Str("hello".to_string())));
        assert!(interpreter.interpret_while_stmt(condition, body).is_ok());
    }

    #[test]
    fn block() {
        let mut interpreter = Interpreter::new();
        let items = vec![
            Item::LetStmt {
                ident: Ident {
                    name: "x".to_string(),
                    range: TextRange { start: 0, end: 0 },
                },
                init: Some(Expr::Literal(Literal::Number(5.0))),
            },
            Item::Block(vec![Item::ExprStmt(Expr::Ident(Ident {
                name: "x".to_string(),
                range: TextRange { start: 0, end: 0 },
            }))]),
        ];
        interpreter.depths.insert(
            Ident {
                name: "x".to_string(),
                range: TextRange { start: 0, end: 0 },
            },
            1,
        );
        assert!(interpreter
            .interpret_block(&items, Env::with_parent(Rc::clone(&interpreter.env)))
            .is_ok());
    }

    #[test]
    fn expr() {
        let mut interpreter = Interpreter::new();

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
        interpreter
            .env
            .borrow_mut()
            .set("x".to_string(), Type::Number(5.0));
        interpreter.depths.insert(
            Ident {
                name: "x".to_string(),
                range: TextRange { start: 0, end: 0 },
            },
            0,
        );
        let result = interpreter.interpret_expr(Expr::Ident(Ident {
            name: "x".to_string(),
            range: TextRange { start: 0, end: 0 },
        }));
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
