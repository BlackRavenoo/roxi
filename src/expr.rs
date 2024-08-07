pub trait Visitor<T> {
    fn visit(expr: &Expr) -> T;
}

pub trait ExprVisitor<T> {
    fn visit_expr(&mut self, expr: &Expr) -> T;
}

#[derive(Clone, Debug)]
pub enum Expr<'a> {
    Assign {
        name: &'a str,
        line: usize,
        value: Box<Expr<'a>>
    },
    Binary {
        values: Box<(Expr<'a>, Expr<'a>)>,
        operator: BinaryOp
    },
    Call {
        line: usize,
        exprs: Vec<Expr<'a>>,
    },
    Literal(Literal<'a>),
    Unary {
        operator: UnaryOp,
        right: Box<Expr<'a>>,
    },
    Ternary {
        exprs: Box<(Expr<'a>, Expr<'a>, Expr<'a>)>,
    },
    Variable {
        name: &'a str,
        line: usize
    },
    Logical {
        values: Box<(Expr<'a>, Expr<'a>)>,
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
pub enum Literal<'a> {
    Number(f64),
    String(&'a str),
    Bool(bool),
    Nil
}