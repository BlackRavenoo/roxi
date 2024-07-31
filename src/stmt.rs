use crate::expr::Expr;

pub trait StmtVisitor<T> {
    fn visit_stmt(&mut self, stmt: Stmt) -> T;
}

#[derive(Clone, Debug)]
pub enum Stmt<'a> {
    Expr(Expr<'a>),
    Print(Expr<'a>),
    Var{
        name: &'a str,
        initializer: Option<Expr<'a>>
    }
}