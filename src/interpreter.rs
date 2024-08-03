use std::fmt::Display;

use crate::{environment::Environment, expr::{BinaryOp, BinaryOpKind, Expr, ExprVisitor, Literal, UnaryOp, UnaryOpKind}, stmt::{Stmt, StmtVisitor}};

#[derive(Debug)]
enum ErrorKind {
    InvalidOperand(&'static str),
    DivisionByZero,
    UninitializedVariable,
    UndefinedVariable
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
            ErrorKind::DivisionByZero => write!(
                f,
                "[line {}] Error: Division by zero.",
                self.line,
            ),
            ErrorKind::UninitializedVariable => write!(
                f,
                "[line {}] Error: Uninitialized variable {}.",
                self.line,
                self.msg
            ),
            ErrorKind::UndefinedVariable => write!(
                f,
                "[line {}] Error: Undefined variable {}.",
                self.line,
                self.msg
            ),
        }
    }
}

type InterpreterResult<T> = Result<T, InterpreterError>;

#[derive(Clone)]
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
    environment: Environment
}

impl ExprVisitor<InterpreterResult<Value>> for Interpreter {
    #[inline]
    fn visit_expr(&mut self, expr: &Expr) -> InterpreterResult<Value> {
        match expr {
            Expr::Binary { left, operator, right } => self.visit_binary_expr(left, operator, right),
            Expr::Grouping { expression } => self.visit_expr(expression),
            Expr::Literal(literal) => match literal {
                Literal::Number(num) => Ok(Value::Number(*num)),
                Literal::String(string) => Ok(Value::String(string.to_string())),
                Literal::Bool(bool) => Ok(Value::Bool(*bool)),
                Literal::Nil => Ok(Value::Nil),
            },
            Expr::Unary { operator, right } => self.visit_unary_expr(operator, right),
            Expr::Ternary { condition, then_branch, else_branch } => self.visit_ternary_expr(condition, then_branch, else_branch),
            Expr::Variable { name, line } => self.visit_var_expr(name, line),
            Expr::Assign { name, value, line } => self.visit_assign_expr(name, value, line),
        }
    }
}

impl StmtVisitor<InterpreterResult<()>> for Interpreter {
    #[inline]
    fn visit_stmt(&mut self, stmt: &Stmt) -> InterpreterResult<()> {
        match stmt {
            Stmt::Expr(expr) => self.visit_expr_stmt(expr),
            Stmt::Print(expr) => self.visit_print_stmt(expr),
            Stmt::Var { name, initializer } => self.visit_var_stmt(name, initializer),
            Stmt::Block { statements } => self.visit_block_stmt(statements),
            Stmt::If { condition, then_branch, else_branch } => self.visit_if_stmt(condition, then_branch, else_branch),
        }
    }
}

impl Interpreter {
    #[inline(always)]
    pub fn new() -> Self {
        Self {
            environment: Environment::new(None)
        }
    }
    pub fn interpret(&mut self, stmt: &Stmt) -> InterpreterResult<()> {
        let result = self.visit_stmt(stmt);
        if let Err(ref e) = result {
            eprintln!("{}", e);
        }

        result
    }

    fn visit_assign_expr(&mut self, name: &str, expr: &Expr, line: &usize) -> InterpreterResult<Value> {
        let value = self.visit_expr(expr)?;
        if self.environment.assign(name, value.clone()) {
            Ok(value)
        } else {
            Err(InterpreterError{
                msg: name.to_owned(),
                line: *line,
                kind: ErrorKind::UndefinedVariable,
            })
        }
    }

    fn visit_unary_expr(&mut self, operator: &UnaryOp, right: &Expr) -> InterpreterResult<Value> {
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

    fn visit_binary_expr(&mut self, left: &Expr, operator: &BinaryOp, right: &Expr) -> InterpreterResult<Value> {
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
                (Value::Number(_), Value::Number(right)) if right == 0.0 => Err(
                    InterpreterError{
                        msg: String::new(),
                        line: operator.line,
                        kind: ErrorKind::DivisionByZero,
                    }
                ),
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

    fn visit_ternary_expr(&mut self, condition: &Expr, then_branch: &Expr, else_branch: &Expr) -> InterpreterResult<Value> {
        let result = self.visit_expr(condition)?;
        self.visit_expr(match Self::is_truthy(&result) {
            true => then_branch,
            false => else_branch,
        })
    }

    fn visit_var_expr(&mut self, name: &str, line: &usize) -> InterpreterResult<Value> {
        match self.environment.get(name) {
            Some(Some(value)) => Ok(value.clone()),
            Some(None) => Err(InterpreterError{
                msg: name.to_owned(),
                line: *line,
                kind: ErrorKind::UninitializedVariable,
            }),
            None => Err(InterpreterError{
                msg: name.to_owned(),
                line: *line,
                kind: ErrorKind::UndefinedVariable,
            }),
        }
    }

    #[inline]
    fn visit_expr_stmt(&mut self, expr: &Expr) -> InterpreterResult<()> {
        let _ = self.visit_expr(expr)?;
        Ok(())
    }

    #[inline]
    fn visit_print_stmt(&mut self, expr: &Expr) -> InterpreterResult<()> {
        let value = self.visit_expr(expr)?;
        println!("{}", value);
        Ok(())
    }
    
    #[inline]
    fn visit_var_stmt(&mut self, name: &str, initializer: &Option<Expr>) -> InterpreterResult<()> {
        let value = if let Some(expr) = initializer {
            Some(self.visit_expr(expr)?)
        } else {
            None
        };
        
        self.environment.define(name, value);
        
        Ok(())
    }
    
    #[inline(always)]
    fn visit_block_stmt(&mut self, statements: &[Stmt]) -> InterpreterResult<()> {
        self.execute_block(statements)
    }

    fn visit_if_stmt(&mut self, condition: &Expr, then_branch: &Stmt, else_branch: &Option<Box<Stmt>>) -> InterpreterResult<()> {
        if Self::is_truthy(&self.visit_expr(condition)?) {
            self.visit_stmt(then_branch)
        } else if let Some(stmt) = &else_branch {
            self.visit_stmt(stmt)
        } else {
            Ok(())
        }
    }

    #[inline(always)]
    fn is_truthy(value: &Value) -> bool {
        match value {
            Value::String(string) => string.len() > 0,
            Value::Bool(bool) => *bool,
            Value::Number(num) => *num != 0.0,
            Value::Nil => false,
        }
    }

    #[inline(always)]
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

    fn execute_block(&mut self, statements: &[Stmt]) -> InterpreterResult<()> {
        // unsafe {
        //     let enclosing = ptr::read(&self.environment);
        //     let env = Environment::new(Some(Box::new(enclosing)));
        //     ptr::write(&mut self.environment, env)
        // }

        let old_env = std::mem::replace(&mut self.environment, Environment::new(None));
        self.environment.enclosing = Some(Box::new(old_env));

        for stmt in statements {
            self.visit_stmt(stmt)?;
        }

        self.environment =  std::mem::take(&mut self.environment.enclosing).map(|e| *e).unwrap();

        Ok(())
    }
}
