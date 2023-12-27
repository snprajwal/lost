use std::collections::HashMap;

use lost_syntax::ast::{Expr, Ident, Item, Source};

use crate::{
    error::{resolution_error, ErrorMsg, Exception},
    stdlib,
};

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum Node {
    Function,
    Method,
    Class,
}

#[derive(Debug, Default)]
pub struct Resolver {
    scopes: Vec<HashMap<String, bool>>,
    depths: HashMap<Ident, usize>,
    node: Option<Node>,
}

impl Resolver {
    pub fn new() -> Self {
        let mut scope = HashMap::default();
        stdlib::init_symbols(&mut scope);
        Self {
            scopes: vec![scope],
            ..Default::default()
        }
    }

    pub fn resolve(&mut self, source: Source) -> Result<HashMap<Ident, usize>, Exception> {
        self.resolve_all(source.items)
            .and_then(|_| Ok(self.depths.clone()))
    }

    pub fn resolve_all(&mut self, items: Vec<Item>) -> Result<(), Exception> {
        items
            .into_iter()
            .try_for_each(|item| self.resolve_item(item))
    }

    fn resolve_item(&mut self, item: Item) -> Result<(), Exception> {
        match item {
            Item::ExprStmt(expr) => self.resolve_expr(expr),
            Item::LetStmt { ident, init } => self.resolve_let_stmt(ident, init),
            Item::IfStmt {
                condition,
                if_item,
                else_item,
            } => self.resolve_if_stmt(condition, *if_item, else_item.map(|item| *item)),
            Item::Block(items) => self.resolve_block(items),
            Item::WhileStmt { condition, body } => self.resolve_while_stmt(condition, *body),
            Item::ReturnStmt(expr) => self.resolve_return_stmt(expr),
            Item::Function {
                ident: name,
                args,
                body,
            } => self.resolve_func_decl(name, args, body),
            Item::Class {
                ident: name,
                methods,
            } => self.resolve_class(name, methods),
        }
    }

    fn resolve_block(&mut self, items: Vec<Item>) -> Result<(), Exception> {
        self.init_scope();
        self.resolve_all(items)?;
        self.end_scope();
        Ok(())
    }

    fn resolve_let_stmt(&mut self, ident: Ident, init: Option<Expr>) -> Result<(), Exception> {
        self.declare(ident.name.clone())?;
        if let Some(expr) = init {
            self.resolve_expr(expr)?;
        }
        self.define(ident.name)?;
        Ok(())
    }

    fn resolve_if_stmt(
        &mut self,
        condition: Expr,
        if_item: Item,
        else_item: Option<Item>,
    ) -> Result<(), Exception> {
        self.resolve_expr(condition)?;
        self.resolve_item(if_item)?;
        if let Some(item) = else_item {
            self.resolve_item(item)?;
        }
        Ok(())
    }

    fn resolve_while_stmt(&mut self, condition: Expr, body: Item) -> Result<(), Exception> {
        self.resolve_expr(condition)?;
        self.resolve_item(body)?;
        Ok(())
    }

    fn resolve_return_stmt(&mut self, expr: Expr) -> Result<(), Exception> {
        if self.node.is_none() {
            return Err(resolution_error(
                ErrorMsg::ReturnOutsideFunction,
                String::default(),
            ));
        }
        self.resolve_expr(expr)
    }

    fn resolve_func_decl(
        &mut self,
        ident: Ident,
        args: Vec<Ident>,
        body: Vec<Item>,
    ) -> Result<(), Exception> {
        self.declare(ident.name.clone())?;
        self.define(ident.name)?;
        self.resolve_function(args, body, Node::Function)?;
        Ok(())
    }

    fn resolve_function(
        &mut self,
        args: Vec<Ident>,
        body: Vec<Item>,
        object: Node,
    ) -> Result<(), Exception> {
        let old = self.node;
        self.node = Some(object);
        self.init_scope();

        for arg in args {
            self.declare(arg.name.clone())?;
            self.define(arg.name)?;
        }
        self.resolve_all(body)?;

        self.end_scope();
        self.node = old;
        Ok(())
    }

    fn resolve_class(&mut self, ident: Ident, methods: Vec<Item>) -> Result<(), Exception> {
        let old = self.node;
        self.node = Some(Node::Class);
        self.declare(ident.name.clone())?;
        self.define(ident.name)?;
        self.init_scope();

        self.define("this".to_string())?;
        for method in methods {
            if let Item::Function { args, body, .. } = method {
                self.resolve_function(args, body, Node::Method)?;
            } else {
                unreachable!("non-functions cannot be passed as methods");
            }
        }

        self.end_scope();
        self.node = old;
        Ok(())
    }

    fn resolve_expr(&mut self, expr: Expr) -> Result<(), Exception> {
        match expr {
            Expr::Assignment { name, value } => self.resolve_assignment(name, *value),
            Expr::Literal(_) => Ok(()),
            Expr::Ident(ident) => self.resolve_ident(ident),
            Expr::Unary { expr, .. } => self.resolve_expr(*expr),
            Expr::Binary { lhs, rhs, .. } | Expr::Logical { lhs, rhs, .. } => self
                .resolve_expr(*lhs)
                .and_then(|_| self.resolve_expr(*rhs)),
            Expr::Group(e) => self.resolve_expr(*e),
            Expr::Call { func, args } => self.resolve_func_call(*func, args),
            Expr::FieldGet { object, .. } => self.resolve_expr(*object),
            Expr::FieldSet { object, value, .. } => self
                .resolve_expr(*object)
                .and_then(|_| self.resolve_expr(*value)),
        }
    }

    fn resolve_ident(&mut self, ident: Ident) -> Result<(), Exception> {
        // The `this` variable cannot be used outside class methods
        if ident.name == "this" && self.node != Some(Node::Method) {
            return Err(resolution_error(
                ErrorMsg::ThisOutsideMethod,
                String::default(),
            ));
        }

        if let Some(&value) = self.scopes.last().and_then(|s| s.get(&ident.name)) {
            if !value {
                return Err(resolution_error(ErrorMsg::SelfIntialiser, ident.name));
            }
        };

        self.resolve_variable(ident)?;

        Ok(())
    }

    fn resolve_assignment(&mut self, ident: Ident, expr: Expr) -> Result<(), Exception> {
        self.resolve_expr(expr)
            .and_then(|_| self.resolve_variable(ident))
    }

    fn resolve_func_call(&mut self, fn_expr: Expr, arg_exprs: Vec<Expr>) -> Result<(), Exception> {
        self.resolve_expr(fn_expr)?;
        for arg in arg_exprs {
            self.resolve_expr(arg)?;
        }
        Ok(())
    }

    fn resolve_variable(&mut self, lit: Ident) -> Result<(), Exception> {
        if let Some((i, _)) = self
            .scopes
            .iter()
            .rev()
            .enumerate()
            .find(|(_, s)| s.contains_key(&lit.name))
        {
            self.depths.insert(lit, i);
            Ok(())
        } else {
            Err(resolution_error(ErrorMsg::UndefinedVar, lit.name))
        }
    }

    fn init_scope(&mut self) {
        self.scopes.push(HashMap::default());
    }

    fn end_scope(&mut self) {
        self.scopes.pop();
    }

    fn declare(&mut self, name: String) -> Result<(), Exception> {
        self.scopes
            .last_mut()
            .ok_or(resolution_error(ErrorMsg::NoScope, name.clone()))
            .map(|s| {
                s.insert(name, false);
            })
    }

    fn define(&mut self, name: String) -> Result<(), Exception> {
        self.scopes
            .last_mut()
            .ok_or(resolution_error(ErrorMsg::NoScope, name.clone()))
            .map(|s| {
                s.insert(name, true);
            })
    }
}
