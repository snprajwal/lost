use std::{
    cell::RefCell,
    collections::HashMap,
    fmt::{Debug, Display},
    rc::Rc,
};

use lost_syntax::ast::Item;

use crate::{
    environment::{Env, Value},
    error::{runtime_error, ErrorMsg, Exception},
    interpret::Interpreter,
};

pub trait Callable {
    fn arity(&self) -> usize;
    fn call(&self, interpreter: &mut Interpreter, args: &[Value]) -> Result<Value, Exception>;
}

#[derive(Clone, Debug, PartialEq)]
pub struct Func {
    pub name: String,
    pub args: Vec<String>,
    pub body: Vec<Item>,
    pub env: Rc<RefCell<Env>>,
}

impl Display for Func {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(&format!("fn {}({})", self.name, self.args.join(", ")))
    }
}

impl Callable for Func {
    fn arity(&self) -> usize {
        self.args.len()
    }
    fn call(&self, interpreter: &mut Interpreter, args: &[Value]) -> Result<Value, Exception> {
        interpreter.call(self, args)
    }
}

#[derive(Clone)]
pub struct NativeFunc {
    pub name: String,
    pub args: Vec<String>,
    pub body: fn(&mut Interpreter, &[Value]) -> Result<Value, Exception>,
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
        f.write_str(&format!(
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
    fn call(&self, interpreter: &mut Interpreter, args: &[Value]) -> Result<Value, Exception> {
        (self.body)(interpreter, args)
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct Class {
    pub name: String,
    pub parent: Option<Box<Class>>,
    pub methods: HashMap<String, Func>,
}

impl Display for Class {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(&format!("class {}", self.name))
    }
}

impl Callable for Class {
    fn arity(&self) -> usize {
        if let Some(init) = self.methods.get(&self.name) {
            init.arity()
        } else {
            0
        }
    }
    fn call(&self, interpreter: &mut Interpreter, args: &[Value]) -> Result<Value, Exception> {
        let instance = Value::Instance(Instance {
            fields: HashMap::default(),
            class: self.clone(),
        });
        self.init(interpreter, args, instance)
    }
}

impl Class {
    pub fn get_method(&self, method: &str) -> Option<Func> {
        self.methods
            .get(method)
            .cloned()
            .or_else(|| self.parent.as_ref().and_then(|p| p.get_method(method)))
    }

    fn init(
        &self,
        interpreter: &mut Interpreter,
        args: &[Value],
        instance: Value,
    ) -> Result<Value, Exception> {
        let initialised = if let Some(p) = &self.parent {
            p.init(interpreter, args, instance)?
        } else {
            instance
        };
        // The constructor has the same name as the class.
        // If it is in the list of defined methods, run it.
        if let Some(mut init) = self.methods.get(&self.name).cloned() {
            init.env = Env::with_parent(init.env);
            init.env.borrow_mut().set("this", initialised);
            init.call(interpreter, args)?;
            Ok(init.env.borrow().get("this")?)
        } else {
            Ok(initialised)
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct Instance {
    pub class: Class,
    fields: HashMap<String, Value>,
}

impl Display for Instance {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(&format!("instance {}", self.class.name))
    }
}

impl Instance {
    pub fn get(self, member: &str) -> Result<Value, Exception> {
        if let Some(value) = self.fields.get(member) {
            return Ok(value.clone());
        }
        // The constructor must not be fetched or explicitly called
        if self.class.name == member {
            return Err(runtime_error(ErrorMsg::GetConstructor, member));
        }
        if let Some(mut func) = self.class.get_method(member) {
            // Add `this` into a parent env for the function to access
            func.env = Env::with_parent(func.env);
            func.env.borrow_mut().set("this", Value::Instance(self));
            return Ok(Value::Func(func));
        }
        Err(runtime_error(ErrorMsg::UndefinedMember, member))
    }

    pub fn set(&mut self, field: &str, value: Value) -> Result<Value, Exception> {
        self.fields.insert(field.to_string(), value.clone());
        Ok(value)
    }
}
