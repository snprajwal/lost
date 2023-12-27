use std::{cell::RefCell, collections::HashMap, rc::Rc};

use log::debug;

use crate::{
    error::{runtime_error, ErrorMsg, Exception},
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
        stdlib::init(&mut env);
        Rc::new(RefCell::new(env))
    }

    pub fn with_parent(parent: Rc<RefCell<Env>>) -> Rc<RefCell<Self>> {
        Rc::new(RefCell::new(Self {
            parent: Some(parent),
            ..Default::default()
        }))
    }

    pub fn set(&mut self, name: String, value: Type) {
        debug!("Set {name} -> {value:?}");
        self.values.insert(name, value);
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
        Err(runtime_error(ErrorMsg::UndefinedVar, name))
    }

    pub fn assign(&mut self, name: String, value: Type) -> Result<(), Exception> {
        debug!("Assign {name} -> {value:?})");
        if self.values.contains_key(&name) {
            return Ok(self.set(name, value));
        }
        if let Some(parent) = &mut self.parent {
            debug!("Assign {name} in parent");
            return parent.borrow_mut().assign(name, value);
        }
        Err(runtime_error(ErrorMsg::UndefinedVar, name))
    }

    pub fn get_at_depth(&self, name: String, depth: usize) -> Result<Type, Exception> {
        debug!("Get {name} at depth {depth}");
        if depth == 0 {
            if let Some(value) = self.values.get(&name) {
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
        name: String,
        value: Type,
        depth: usize,
    ) -> Result<(), Exception> {
        debug!("Set {name} -> {value} at depth {depth}");
        if depth == 0 {
            if self.values.contains_key(&name) {
                return Ok(self.set(name, value));
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
