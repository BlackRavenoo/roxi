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
        initializer: Option<Expr>,
        line: usize,
        offset: usize
    },
    Block {
        statements: Vec<Stmt>
    },
    Class {
        name: String,
        methods: Vec<Stmt>,
        static_methods: Vec<Stmt>,
        getters: Vec<Stmt>,
        line: usize,
        offset: usize
    },
    Function {
        name: String,
        params: Vec<(String, usize)>,
        body: Vec<Stmt>,
        line: usize,
        offset: usize
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