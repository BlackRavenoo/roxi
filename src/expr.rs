pub trait Visitor<T> {
    fn visit(expr: &Expr) -> T;
}

pub trait ExprVisitor<T> {
    fn visit_expr(&mut self, expr: &Expr) -> T;
}

#[derive(Clone, Debug)]
pub enum Expr {
    Assign {
        name: String,
        line: usize,
        value: Box<Expr>
    },
    Binary {
        values: Box<(Expr, Expr)>,
        operator: BinaryOp
    },
    Call {
        line: usize,
        exprs: Vec<Expr>,
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
        line: usize
    },
    Logical {
        values: Box<(Expr, Expr)>,
        operator: BinaryOp
    }
}

#[derive(Clone, Debug)]
pub struct BinaryOp {
    pub line: usize,
    pub kind: BinaryOpKind
}

#[derive(Clone, Debug)]
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

impl BinaryOpKind {
    pub fn as_str(&self) -> &'static str {
        match self {
            BinaryOpKind::Add => "+",
            BinaryOpKind::Sub => "-",
            BinaryOpKind::Mul => "*",
            BinaryOpKind::Div => "/",
            BinaryOpKind::Eq => "==",
            BinaryOpKind::Ne => "!=",
            BinaryOpKind::Lt => "<",
            BinaryOpKind::Le => "<=",
            BinaryOpKind::Gt => ">",
            BinaryOpKind::Ge => ">=",
            BinaryOpKind::And => "and",
            BinaryOpKind::Or => "or",
        }
    }
}

#[derive(Clone, Debug)]
pub enum UnaryOpKind {
    Neg,
    Not
}

impl UnaryOpKind {
    pub fn as_str(&self) -> &'static str {
        match self {
            UnaryOpKind::Neg => "-",
            UnaryOpKind::Not => "!",
        }
    }
}

#[derive(Clone, Debug)]
pub enum Literal {
    Number(f64),
    String(String),
    Bool(bool),
    Nil
}