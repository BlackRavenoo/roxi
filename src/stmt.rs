use crate::expr::Expr;

pub trait StmtVisitor<T> {
    fn visit_stmt(&mut self, stmt: &Stmt) -> T;
}

#[derive(Clone, Debug, PartialEq)]
pub enum Stmt {
    Expr(Expr),
    Print(Expr),
    Return {
        line: usize,
        value: Expr
    },
    Var {
        name: String,
        initializer: Option<Expr>
    },
    Block {
        statements: Vec<Stmt>
    },
    Function {
        name: String,
        params: Vec<String>,
        body: Vec<Stmt>
    },
    If {
        condition: Expr,
        then_branch: Box<Stmt>,
        else_branch: Option<Box<Stmt>>
    },
    While {
        condition: Expr,
        body: Box<Stmt>
    },
    Break {
        line: usize 
    }
}