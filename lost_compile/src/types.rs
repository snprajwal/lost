use std::fmt::{Debug, Display};

use lost_syntax::ast::Item;

use crate::{environment::Env, error::Exception, interpret::Interpreter};

#[derive(Clone, Debug, PartialEq)]
pub enum Type {
    Boolean(bool),
    Number(f64),
    Str(String),
    Func(Func),
    NativeFunc(NativeFunc),
    Null,
}

impl Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(&match self {
            Self::Boolean(b) => b.to_string(),
            Self::Number(n) => n.to_string(),
            Self::Str(s) => s.to_owned(),
            Self::Func(f) => f.to_string(),
            Self::NativeFunc(f) => f.to_string(),
            Self::Null => "null".to_string(),
        })
    }
}

pub trait Callable {
    fn arity(&self) -> usize;
    fn call(self, interpreter: &mut Interpreter, args: Vec<Type>) -> Result<Type, Exception>;
}

#[derive(Clone, Debug, PartialEq)]
pub struct Func {
    pub name: String,
    pub args: Vec<String>,
    pub body: Vec<Item>,
    pub env: Env,
}

impl Display for Func {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_fmt(format_args!("fn {}({})", self.name, self.args.join(", ")))
    }
}

impl Callable for Func {
    fn arity(&self) -> usize {
        self.args.len()
    }
    fn call(self, interpreter: &mut Interpreter, args: Vec<Type>) -> Result<Type, Exception> {
        interpreter.call_func(self.clone(), args, self.env)
    }
}

#[derive(Clone)]
pub struct NativeFunc {
    pub name: String,
    pub args: Vec<String>,
    pub body: fn(&mut Interpreter, Vec<Type>) -> Result<Type, Exception>,
}

impl PartialEq for NativeFunc {
    fn eq(&self, other: &Self) -> bool {
        self.name == other.name && self.args == other.args
    }
}

impl Debug for NativeFunc {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("NativeFunc")
            .field("name", &self.name)
            .field("args", &self.args)
            .finish()
    }
}

impl Display for NativeFunc {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_fmt(format_args!(
            "native fn {}({})",
            self.name,
            self.args.join(", ")
        ))
    }
}

impl Callable for NativeFunc {
    fn arity(&self) -> usize {
        self.args.len()
    }
    fn call(self, interpreter: &mut Interpreter, args: Vec<Type>) -> Result<Type, Exception> {
        (self.body)(interpreter, args)
    }
}
