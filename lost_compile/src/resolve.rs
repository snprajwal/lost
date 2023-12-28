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
}

#[derive(Debug, Default)]
pub struct Resolver {
    scopes: Vec<HashMap<String, bool>>,
    depths: HashMap<Ident, usize>,
    node: Option<Node>,
    is_child_class: bool,
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

    pub fn resolve(&mut self, source: &Source) -> Result<HashMap<Ident, usize>, Exception> {
        self.resolve_all(&source.items)
            .and_then(|_| Ok(self.depths.clone()))
    }

    pub fn resolve_all(&mut self, items: &[Item]) -> Result<(), Exception> {
        items.iter().try_for_each(|item| self.resolve_item(item))
    }

    fn resolve_item(&mut self, item: &Item) -> Result<(), Exception> {
        match item {
            Item::ExprStmt(expr) => self.resolve_expr(expr),
            Item::LetStmt { ident, init } => self.resolve_let_stmt(ident, init),
            Item::IfStmt {
                condition,
                if_item,
                else_item,
            } => self.resolve_if_stmt(condition, if_item, else_item),
            Item::Block(items) => self.resolve_block(items),
            Item::WhileStmt { condition, body } => self.resolve_while_stmt(condition, body),
            Item::ReturnStmt(expr) => self.resolve_return_stmt(expr),
            Item::Function { ident, args, body } => self.resolve_func_decl(ident, args, body),
            Item::Class {
                ident,
                parent,
                methods,
            } => self.resolve_class(ident, parent, methods),
        }
    }

    fn resolve_block(&mut self, items: &[Item]) -> Result<(), Exception> {
        self.init_scope();
        self.resolve_all(items)?;
        self.end_scope();
        Ok(())
    }

    fn resolve_let_stmt(&mut self, ident: &Ident, init: &Option<Expr>) -> Result<(), Exception> {
        self.declare(&ident.name)?;
        if let Some(expr) = init {
            self.resolve_expr(expr)?;
        }
        self.define(&ident.name)?;
        Ok(())
    }

    fn resolve_if_stmt(
        &mut self,
        condition: &Expr,
        if_item: &Item,
        else_item: &Option<Box<Item>>,
    ) -> Result<(), Exception> {
        self.resolve_expr(condition)?;
        self.resolve_item(if_item)?;
        if let Some(item) = else_item {
            self.resolve_item(item)?;
        }
        Ok(())
    }

    fn resolve_while_stmt(&mut self, condition: &Expr, body: &Item) -> Result<(), Exception> {
        self.resolve_expr(condition)?;
        self.resolve_item(body)?;
        Ok(())
    }

    fn resolve_return_stmt(&mut self, expr: &Expr) -> Result<(), Exception> {
        if self.node.is_none() {
            return Err(resolution_error(ErrorMsg::ReturnOutsideFunction, ""));
        }
        self.resolve_expr(expr)
    }

    fn resolve_func_decl(
        &mut self,
        ident: &Ident,
        args: &[Ident],
        body: &[Item],
    ) -> Result<(), Exception> {
        self.declare(&ident.name)?;
        self.define(&ident.name)?;
        self.resolve_function(args, body, Node::Function)?;
        Ok(())
    }

    fn resolve_function(
        &mut self,
        args: &[Ident],
        body: &[Item],
        object: Node,
    ) -> Result<(), Exception> {
        let old = self.node;
        self.node = Some(object);
        self.init_scope();

        for arg in args {
            self.declare(&arg.name)?;
            self.define(&arg.name)?;
        }
        self.resolve_all(body)?;

        self.end_scope();
        self.node = old;
        Ok(())
    }

    fn resolve_class(
        &mut self,
        ident: &Ident,
        parent: &Option<Ident>,
        methods: &[Item],
    ) -> Result<(), Exception> {
        self.declare(&ident.name)?;
        self.define(&ident.name)?;

        if let Some(p) = &parent {
            if p.name == ident.name {
                return Err(resolution_error(ErrorMsg::SelfInherit, &p.name));
            }
            self.is_child_class = true;
            self.resolve_ident(&p)?;
            self.init_scope();
            self.define(&"super".to_string())?;
        }

        self.init_scope();
        self.define(&"this".to_string())?;
        for method in methods {
            if let Item::Function { args, body, .. } = method {
                self.resolve_function(args, body, Node::Method)?;
            } else {
                unreachable!("non-functions cannot be passed as methods");
            }
        }
        self.end_scope();
        if parent.is_some() {
            self.end_scope();
        }

        Ok(())
    }

    fn resolve_expr(&mut self, expr: &Expr) -> Result<(), Exception> {
        match expr {
            Expr::Assignment { name, value } => self.resolve_assignment(name, value),
            Expr::Literal(_) => Ok(()),
            Expr::Ident(ident) => self.resolve_ident(ident),
            Expr::Unary { expr, .. } => self.resolve_expr(expr),
            Expr::Binary { lhs, rhs, .. } | Expr::Logical { lhs, rhs, .. } => {
                self.resolve_expr(lhs).and_then(|_| self.resolve_expr(rhs))
            }
            Expr::Group(e) => self.resolve_expr(e),
            Expr::Call { func, args } => self.resolve_func_call(func, args),
            Expr::FieldGet { object, .. } => self.resolve_expr(object),
            Expr::FieldSet { object, value, .. } => self
                .resolve_expr(object)
                .and_then(|_| self.resolve_expr(value)),
            Expr::Super(super_, _) => self.resolve_ident(super_),
        }
    }

    fn resolve_ident(&mut self, ident: &Ident) -> Result<(), Exception> {
        // The `this` variable cannot be used outside class methods
        match ident.name.as_str() {
            "this" => {
                if self.node != Some(Node::Method) {
                    return Err(resolution_error(ErrorMsg::ThisOutsideMethod, ""));
                }
            }
            "super" => {
                if self.node != Some(Node::Method) {
                    return Err(resolution_error(ErrorMsg::SuperOutsideMethod, ""));
                }
                if !self.is_child_class {
                    return Err(resolution_error(ErrorMsg::SuperWithoutParent, ""));
                }
            }
            _ => (),
        }

        if let Some(&value) = self.scopes.last().and_then(|s| s.get(&ident.name)) {
            if !value {
                return Err(resolution_error(ErrorMsg::SelfInitialiser, &ident.name));
            }
        };

        self.resolve_variable(ident)?;

        Ok(())
    }

    fn resolve_assignment(&mut self, ident: &Ident, expr: &Expr) -> Result<(), Exception> {
        self.resolve_expr(expr)
            .and_then(|_| self.resolve_variable(ident))
    }

    fn resolve_func_call(&mut self, fn_expr: &Expr, arg_exprs: &[Expr]) -> Result<(), Exception> {
        self.resolve_expr(fn_expr)?;
        for arg in arg_exprs {
            self.resolve_expr(arg)?;
        }
        Ok(())
    }

    fn resolve_variable(&mut self, lit: &Ident) -> Result<(), Exception> {
        if let Some((i, _)) = self
            .scopes
            .iter()
            .rev()
            .enumerate()
            .find(|(_, s)| s.contains_key(&lit.name))
        {
            self.depths.insert(lit.clone(), i);
            Ok(())
        } else {
            Err(resolution_error(ErrorMsg::UndefinedVar, &lit.name))
        }
    }

    fn init_scope(&mut self) {
        self.scopes.push(HashMap::default());
    }

    fn end_scope(&mut self) {
        self.scopes.pop();
    }

    fn declare(&mut self, name: &str) -> Result<(), Exception> {
        self.scopes
            .last_mut()
            .ok_or_else(|| resolution_error(ErrorMsg::NoScope, &name))
            .map(|s| {
                s.insert(name.to_string(), false);
            })
    }

    fn define(&mut self, name: &str) -> Result<(), Exception> {
        self.scopes
            .last_mut()
            .ok_or_else(|| resolution_error(ErrorMsg::NoScope, &name))
            .map(|s| {
                s.insert(name.to_string(), true);
            })
    }
}
