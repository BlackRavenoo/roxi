use std::fmt::Display;

use crate::expr::{BinaryOp, BinaryOpKind, Expr, ExprVisitor, Literal, UnaryOp, UnaryOpKind};

#[derive(Debug)]
enum ErrorKind {
    InvalidOperand(&'static str)
}

#[derive(Debug)]
pub struct InterpreterError {
    msg: String,
    line: usize,
    kind: ErrorKind
}

impl Display for InterpreterError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.kind {
            ErrorKind::InvalidOperand(expected) => write!(
                f,
                "[line {}] Error: Invalid operand. Found {}. Expected {}.",
                self.line,
                self.msg,
                expected
            ),
        }
    }
}

type InterpreterResult<T> = Result<T, InterpreterError>;

pub enum Value {
    Number(f64),
    String(String),
    Bool(bool),
    Nil
}

impl Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Number(num) => write!(f, "{}", num),
            Value::String(str) => write!(f, "{}", str),
            Value::Bool(bool) => write!(f, "{}", bool),
            Value::Nil => write!(f, "nil"),
        }
    }
}


pub struct Interpreter {

}

impl ExprVisitor<InterpreterResult<Value>> for Interpreter {
    fn visit_expr(&mut self, expr: Expr) -> InterpreterResult<Value> {
        match expr {
            Expr::Binary { left, operator, right } => self.visit_binary_expr(*left, operator, *right),
            Expr::Grouping { expression } => self.visit_expr(*expression),
            Expr::Literal(literal) => match literal {
                Literal::Number(num) => Ok(Value::Number(num)),
                Literal::String(string) => Ok(Value::String(string.to_string())),
                Literal::Bool(bool) => Ok(Value::Bool(bool)),
                Literal::Nil => Ok(Value::Nil),
            },
            Expr::Unary { operator, right } => self.visit_unary_expr(operator, *right),
            Expr::Ternary { condition, then_branch, else_branch } => todo!(),
        }
    }
}

impl Interpreter {
    pub fn interpret(&mut self, expr: Expr) -> InterpreterResult<Value> {
        let result = self.visit_expr(expr);
        if let Err(ref e) = result {
            eprintln!("{}", e);
        }

        result
    }

    fn visit_unary_expr(&mut self, operator: UnaryOp, right: Expr) -> InterpreterResult<Value> {
        let right = self.visit_expr(right)?;
        
        match operator.kind {
            UnaryOpKind::Neg => {
                match right {
                    Value::Number(num) => Ok(Value::Number(-num)),
                    v => Self::invalid_operand(&v, "number", operator.line)
                }
            },
            UnaryOpKind::Not => Ok(Value::Bool(!Self::is_truthy(&right))),
        }
    }

    fn visit_binary_expr(&mut self, left: Expr, operator: BinaryOp, right: Expr) -> InterpreterResult<Value> {
        let (left, right) = (self.visit_expr(left)?, self.visit_expr(right)?);

        match operator.kind {
            BinaryOpKind::Add => match (&left, &right) {
                (Value::Number(left), Value::Number(right)) => Ok(Value::Number(left + right)),
                (Value::String(_), _) | (_, Value::String(_)) => Ok(Value::String(format!("{}{}", left, right))),
                (Value::Number(_), v) => Self::invalid_operand(&v, "number or string", operator.line),
                (Value::Bool(_), v) | (Value::Nil, v) => Self::invalid_operand(&v, "string", operator.line),
            },
            BinaryOpKind::Sub => match (left, right) {
                (Value::Number(left), Value::Number(right)) => Ok(Value::Number(left - right)),
                (Value::Number(_), v) => Self::invalid_operand(&v, "number", operator.line),
                (v, _) => Self::invalid_operand(&v, "number", operator.line)
            },
            BinaryOpKind::Mul => match (left, right) {
                (Value::Number(left), Value::Number(right)) => Ok(Value::Number(left * right)),
                (Value::Number(_), v) => Self::invalid_operand(&v, "number", operator.line),
                (v, _) => Self::invalid_operand(&v, "number", operator.line)
            },
            BinaryOpKind::Div => match (left, right) {
                (Value::Number(_), Value::Number(right)) if right == 0.0 => todo!(),
                (Value::Number(left), Value::Number(right)) => Ok(Value::Number(left / right)),
                (Value::Number(_), v) => Self::invalid_operand(&v, "number", operator.line),
                (v, _) => Self::invalid_operand(&v, "number", operator.line)
            },
            BinaryOpKind::Eq => Ok(Value::Bool(Self::is_equal(&left, &right))),
            BinaryOpKind::Ne => Ok(Value::Bool(!Self::is_equal(&left, &right))),
            BinaryOpKind::Lt => match (left, right) {
                (Value::Number(left), Value::Number(right)) => Ok(Value::Bool(left < right)),
                (Value::Number(_), v) => Self::invalid_operand(&v, "number", operator.line),
                (v, _) => Self::invalid_operand(&v, "number", operator.line)
            },
            BinaryOpKind::Le => match (left, right) {
                (Value::Number(left), Value::Number(right)) => Ok(Value::Bool(left <= right)),
                (Value::Number(_), v) => Self::invalid_operand(&v, "number", operator.line),
                (v, _) => Self::invalid_operand(&v, "number", operator.line)
            },
            BinaryOpKind::Gt => match (left, right) {
                (Value::Number(left), Value::Number(right)) => Ok(Value::Bool(left > right)),
                (Value::Number(_), v) => Self::invalid_operand(&v, "number", operator.line),
                (v, _) => Self::invalid_operand(&v, "number", operator.line)
            },
            BinaryOpKind::Ge => match (left, right) {
                (Value::Number(left), Value::Number(right)) => Ok(Value::Bool(left >= right)),
                (Value::Number(_), v) => Self::invalid_operand(&v, "number", operator.line),
                (v, _) => Self::invalid_operand(&v, "number", operator.line)
            },
            BinaryOpKind::And => Ok(Value::Bool(Self::is_truthy(&left) && Self::is_truthy(&right))),
            BinaryOpKind::Or => Ok(Value::Bool(Self::is_truthy(&left) || Self::is_truthy(&right))),
        }
    }

    fn is_truthy(value: &Value) -> bool {
        match value {
            Value::String(string) => string.len() > 0,
            Value::Bool(bool) => *bool,
            Value::Number(num) => *num != 0.0,
            Value::Nil => false,
        }
    }

    fn is_equal(left: &Value, right: &Value) -> bool {
        match (left, right) {
            (Value::Number(left), Value::Number(right)) => left == right,
            (Value::String(left), Value::String(right)) => left == right,
            (Value::Bool(left), Value::Bool(right)) => left == right,
            (Value::Nil, Value::Nil) => true,
            _ => false
        }
    }

    fn invalid_operand(found: &Value, expected: &'static str, line: usize) -> InterpreterResult<Value> {
        let msg = match found {
            Value::Number(num) => format!("number: {}", num),
            Value::String(str) => format!("string: \"{}\"", str),
            Value::Bool(bool) => format!("bool: {}", bool),
            Value::Nil => String::from("nil"),
        };
        Err(
            InterpreterError {
                msg,
                line,
                kind: ErrorKind::InvalidOperand(expected),
            }
        )
    }
}
