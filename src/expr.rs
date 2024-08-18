use crate::stmt::Stmt;

pub trait ExprVisitor<T> {
    fn visit_expr(&mut self, expr: &Expr) -> T;
}

#[derive(Clone, Debug, PartialEq)]
pub enum Expr {
    Assign {
        name: String,
        line: usize,
        value: Box<Expr>,
        offset: usize
    },
    Binary {
        values: Box<(Expr, Expr)>,
        operator: BinaryOp
    },
    Call {
        line: usize,
        exprs: Vec<Expr>,
    },
    Get {
        name: String,
        object: Box<Expr>,
        line: usize
    },
    Set {
        name: String,
        object: Box<Expr>,
        value: Box<Expr>,
        line: usize
    },
    Super {
        method: String,
        offset: usize,
        line: usize
    },
    This {
        line: usize,
        offset: usize
    },
    Literal(Literal),
    Unary {
        operator: UnaryOp,
        right: Box<Expr>,
    },
    Ternary {
        exprs: Box<(Expr, Expr, Expr)>,
    },
    Variable {
        name: String,
        line: usize,
        offset: usize,
    },
    Logical {
        values: Box<(Expr, Expr)>,
        operator: BinaryOp
    },
    Lambda {
        params: Vec<(String, usize)>,
        body: Vec<Stmt>
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct BinaryOp {
    pub line: usize,
    pub kind: BinaryOpKind
}

#[derive(Clone, Debug, PartialEq)]
pub struct UnaryOp {
    pub line: usize,
    pub kind: UnaryOpKind
}

#[derive(Clone, Debug, PartialEq)]
pub enum BinaryOpKind {
    Add,
    Sub,
    Mul,
    Div,
    Eq,
    Ne,
    Lt,
    Le,
    Gt,
    Ge,
    And,
    Or
}

#[derive(Clone, Debug, PartialEq)]
pub enum UnaryOpKind {
    Neg,
    Not
}

#[derive(Clone, Debug, PartialEq)]
pub enum Literal {
    Number(f64),
    String(String),
    Bool(bool),
    Nil
}