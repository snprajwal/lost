use std::{cell::RefCell, collections::HashMap, fmt::Display, rc::Rc};

use log::debug;

use crate::{
    error::{runtime_error, ErrorMsg, Exception},
    stdlib,
    types::{Class, Func, Instance, NativeFunc},
};

#[derive(Clone, Debug, PartialEq)]
pub enum Value {
    Boolean(bool),
    Number(f64),
    Str(String),
    Func(Func),
    NativeFunc(NativeFunc),
    Class(Class),
    Instance(Instance),
    Null,
}

impl Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(&match self {
            Self::Boolean(b) => b.to_string(),
            Self::Number(n) => n.to_string(),
            Self::Str(s) => s.clone(),
            Self::Func(f) => f.to_string(),
            Self::NativeFunc(f) => f.to_string(),
            Self::Class(c) => c.to_string(),
            Self::Instance(i) => i.to_string(),
            Self::Null => "null".to_string(),
        })
    }
}

#[derive(Debug, Default, Clone, PartialEq)]
pub struct Env {
    values: HashMap<String, Value>,
    pub parent: Option<Rc<RefCell<Env>>>,
}

impl Env {
    pub fn new() -> Rc<RefCell<Self>> {
        let mut env = Self::default();
        stdlib::init(&mut env);
        Rc::new(RefCell::new(env))
    }

    pub fn with_parent(parent: Rc<RefCell<Env>>) -> Rc<RefCell<Self>> {
        Rc::new(RefCell::new(Self {
            parent: Some(parent),
            ..Default::default()
        }))
    }

    pub fn set(&mut self, name: &str, value: Value) {
        debug!("Set {name} -> {value:?}");
        self.values.insert(name.to_string(), value);
    }

    pub fn get(&self, name: &str) -> Result<Value, Exception> {
        debug!("Get {name}");
        if let Some(value) = self.values.get(name) {
            return Ok(value.clone());
        }
        if let Some(parent) = &self.parent {
            debug!("Get {name} from parent");
            return parent.borrow().get(name);
        }
        Err(runtime_error(ErrorMsg::UndefinedVar, name))
    }

    pub fn assign(&mut self, name: &str, value: Value) -> Result<(), Exception> {
        debug!("Assign {name} -> {value:?})");
        if self.values.contains_key(name) {
            self.set(name, value);
            return Ok(());
        }
        if let Some(parent) = &mut self.parent {
            debug!("Assign {name} in parent");
            return parent.borrow_mut().assign(name, value);
        }
        Err(runtime_error(ErrorMsg::UndefinedVar, name))
    }

    pub fn get_at_depth(&self, name: &str, depth: usize) -> Result<Value, Exception> {
        debug!("Get {name} at depth {depth}");
        if depth == 0 {
            if let Some(value) = self.values.get(name) {
                return Ok(value.clone());
            }
            return Err(runtime_error(ErrorMsg::MisresolvedVar, name));
        }
        self.parent
            .as_ref()
            .expect("depth exceeds maximum environment depth")
            .borrow()
            .get_at_depth(name, depth - 1)
    }

    pub fn assign_at_depth(
        &mut self,
        name: &str,
        value: Value,
        depth: usize,
    ) -> Result<(), Exception> {
        debug!("Set {name} -> {value} at depth {depth}");
        if depth == 0 {
            if self.values.contains_key(name) {
                self.set(name, value);
                return Ok(());
            }
            return Err(runtime_error(ErrorMsg::MisresolvedVar, name));
        }
        self.parent
            .as_ref()
            .expect("depth exceeds maximum environment depth")
            .borrow_mut()
            .assign_at_depth(name, value, depth - 1)
    }
}
