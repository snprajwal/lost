use lost_syntax::ast::{BinOp, Expr, Ident, Item, Literal, Source, UnaryOp};

use crate::chunk::{Chunk, Object, Op, Value};

#[derive(Default, Debug)]
pub struct Generator {
    chunk: Chunk,
}

impl Generator {
    pub fn generate(&mut self, source: &Source) -> Chunk {
        self.generate_all(&source.items);
        std::mem::take(&mut self.chunk)
    }

    pub fn generate_all(&mut self, items: &[Item]) {
        items.iter().for_each(|item| self.generate_item(item))
    }

    fn generate_item(&mut self, item: &Item) {
        match item {
            Item::ExprStmt(expr) => self.generate_expr_stmt(expr),
            Item::LetStmt { ident, init } => self.generate_let_stmt(ident, init),
            // Item::IfStmt {
            //     condition,
            //     if_item,
            //     else_item,
            // } => self.generate_if_stmt(condition, if_item, else_item),
            // Item::Block(items) => self.generate_block(items),
            // Item::WhileStmt { condition, body } => self.generate_while_stmt(condition, body),
            // Item::ReturnStmt(expr) => self.generate_return_stmt(expr),
            // Item::Function { ident, args, body } => self.generate_func_decl(ident, args, body),
            // Item::Class {
            //     ident,
            //     parent,
            //     methods,
            // } => self.generate_class(ident, parent, methods),
            _ => todo!(),
        }
    }

    fn generate_expr_stmt(&mut self, expr: &Expr) {
        self.generate_expr(expr);
        self.chunk.add_op(Op::Pop);
    }

    fn generate_let_stmt(&mut self, ident: &Ident, init: &Option<Expr>) {}

    fn generate_expr(&mut self, expr: &Expr) {
        match expr {
            // Expr::Assignment { name, value } => self.generate_assignment(name, value),
            Expr::Literal(lit) => self.generate_literal(lit),
            // Expr::Ident(ident) => self.generate_ident(ident),
            Expr::Unary { op, expr } => self.generate_unary(op, expr),
            Expr::Binary { lhs, op, rhs } => self.generate_binary(lhs, op, rhs),
            // Expr::Logical { lhs, op, rhs } => self.generate_logical(lhs, op, rhs),
            Expr::Group(e) => self.generate_expr(e),
            // Expr::Call { func, args } => self.generate_call(func, args),
            // Expr::FieldGet { object, field } => self.generate_field_get(object, field),
            // Expr::FieldSet {
            //     object,
            //     field,
            //     value,
            // } => self.generate_field_set(object, field, value),
            // Expr::Super(super_, method) => self.generate_super(super_, method),
            _ => (),
        }
    }

    fn generate_literal(&mut self, lit: &Literal) {
        match lit.clone() {
            Literal::Number(n) => self.chunk.add_constant(Value::Number(n)),
            Literal::Str(s) => self.chunk.add_object(Object::Str(s)),
            Literal::Boolean(b) => self.chunk.add_op(if b { Op::True } else { Op::False }),
            Literal::Null => self.chunk.add_op(Op::Null),
        };
    }

    fn generate_unary(&mut self, op: &UnaryOp, expr: &Expr) {
        self.generate_expr(expr);
        match op {
            UnaryOp::Bang => self.chunk.add_op(Op::Not),
            UnaryOp::Minus => self.chunk.add_op(Op::Negate),
            UnaryOp::Increment => todo!(),
            UnaryOp::Decrement => todo!(),
        };
    }

    fn generate_binary(&mut self, lhs: &Expr, op: &BinOp, rhs: &Expr) {
        self.generate_expr(lhs);
        self.generate_expr(rhs);

        match op {
            BinOp::Slash => self.chunk.add_op(Op::Div),
            BinOp::Star => self.chunk.add_op(Op::Mul),
            BinOp::Plus => self.chunk.add_op(Op::Add),
            BinOp::Minus => self.chunk.add_op(Op::Sub),
            BinOp::Modulo => self.chunk.add_op(Op::Mod),
            BinOp::Greater => self.chunk.add_op(Op::Greater),
            BinOp::GreaterEqual => {
                self.chunk.add_op(Op::Less);
                self.chunk.add_op(Op::Not);
            }
            BinOp::Less => self.chunk.add_op(Op::Less),
            BinOp::LessEqual => {
                self.chunk.add_op(Op::Greater);
                self.chunk.add_op(Op::Not);
            }
            BinOp::BangEqual => {
                self.chunk.add_op(Op::Equal);
                self.chunk.add_op(Op::Not);
            }
            BinOp::EqualEqual => self.chunk.add_op(Op::Equal),
        };
    }
}
