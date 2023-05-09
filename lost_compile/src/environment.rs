use std::collections::HashMap;

use log::debug;
use lost_syntax::ast::Literal;

use crate::{
    error::{make, ErrorMsg, Exception},
    types::Type,
};

#[derive(Debug, Default, Clone, PartialEq)]
pub struct Env {
    values: HashMap<String, Type>,
    pub parent: Option<Box<Env>>,
}

impl Env {
    pub fn with_parent(parent: Env) -> Self {
        Self {
            parent: Some(Box::new(parent)),
            ..Default::default()
        }
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
            return parent.get(name);
        }
        Err(make(ErrorMsg::UndefinedVar, name))
    }

    pub fn assign(&mut self, name: String, value: Type) -> Result<Type, Exception> {
        debug!("Assign {name} -> {value:?})");
        if self.values.contains_key(&name) {
            return Ok(self.set(name, value));
        }
        if let Some(parent) = &mut self.parent {
            return parent.assign(name, value);
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
