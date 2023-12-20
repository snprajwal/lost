use std::{cell::RefCell, collections::HashMap, rc::Rc};

use log::debug;
use lost_syntax::ast::Literal;

use crate::{
    error::{make, ErrorMsg, Exception},
    stdlib,
    types::Type,
};

#[derive(Debug, Default, Clone, PartialEq)]
pub struct Env {
    values: HashMap<String, Type>,
    pub parent: Option<Rc<RefCell<Env>>>,
}

impl Env {
    pub fn new() -> Rc<RefCell<Self>> {
        let mut env = Self::default();
        stdlib::init_io(&mut env);
        Rc::new(RefCell::new(env))
    }

    pub fn with_parent(parent: Rc<RefCell<Env>>) -> Rc<RefCell<Self>> {
        Rc::new(RefCell::new(Self {
            parent: Some(parent),
            ..Default::default()
        }))
    }

    pub fn set(&mut self, name: String, value: Type) -> Type {
        debug!("Set {name} -> {value:?}");
        self.values.insert(name, value.clone());
        value
    }

    pub fn get(&self, name: String) -> Result<Type, Exception> {
        debug!("Get {name}");
        if let Some(value) = self.values.get(&name) {
            return Ok(value.clone());
        }
        if let Some(parent) = &self.parent {
            debug!("Get {name} from parent");
            return parent.borrow().get(name);
        }
        Err(make(ErrorMsg::UndefinedVar, name))
    }

    pub fn assign(&mut self, name: String, value: Type) -> Result<Type, Exception> {
        debug!("Assign {name} -> {value:?})");
        if self.values.contains_key(&name) {
            return Ok(self.set(name, value));
        }
        if let Some(parent) = &mut self.parent {
            debug!("Assign {name} in parent");
            return parent.borrow_mut().assign(name, value);
        }
        Err(make(ErrorMsg::UndefinedVar, name))
    }

    pub fn from_literal(&self, value: Literal) -> Result<Type, Exception> {
        Ok(match value {
            Literal::Str(s) => Type::Str(s),
            Literal::Number(n) => Type::Number(n),
            Literal::Boolean(b) => Type::Boolean(b),
            Literal::Ident(name) => return self.get(name),
            Literal::Null => Type::Null,
        })
    }
}
