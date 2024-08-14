use std::{cell::RefCell, collections::HashMap, fmt::Display, rc::Rc};

use crate::{environment::{GlobalEnvironment, LocalEnvironment}, expr::{BinaryOp, BinaryOpKind, Expr, ExprVisitor, Literal, UnaryOp, UnaryOpKind}, resolver::Binding, stmt::{Stmt, StmtVisitor}};

#[derive(Debug)]
enum ErrorKind {
    InvalidOperand(&'static str),
    DivisionByZero,
    UninitializedVariable,
    UndefinedVariable,
    NotCallable,
    WrongArity(usize),
    Return(Value),
    Break
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
            ErrorKind::NotCallable => write!(
                f,
                "[line {}] Error: Can only call functions and classes.",
                self.line
            ),
            ErrorKind::WrongArity(c) => write!(
                f,
                "[line {}] Error: Expected {} arguments, but got {}.",
                self.line,
                self.msg,
                c
            ),
            ErrorKind::Break => write!(
                f,
                "[line {}] Error: `break` executed outside a enclosing loop.",
                self.line
            ),
            ErrorKind::Return(_) => write!(
                f,
                "[line {}] Error: `return` outside function.",
                self.line
            ),
        }
    }
}

pub type InterpreterResult<T> = Result<T, InterpreterError>;

#[derive(Clone, Debug)]
pub enum Value {
    Number(f64),
    String(String),
    Bool(bool),
    NativeFunction {
        arity: u16,
        fun: fn(&mut Interpreter, Vec<Value>) -> Value
    },
    Function {
        params: Vec<(String, usize)>,
        body: Vec<Stmt>,
        closure: Option<Rc<RefCell<LocalEnvironment>>>
    },
    Nil,
}

impl Value {
    pub fn arity(&self, line: &usize) -> InterpreterResult<u16> {
        match self {
            Value::NativeFunction { arity,.. } => Ok(*arity),
            Value::Function { params, .. } => Ok(params.len() as u16),
            _ => Err(InterpreterError {
                msg: String::new(),
                line: *line,
                kind: ErrorKind::NotCallable,
            })
        }
    }

    pub fn call(self, line: &usize, interpreter: &mut Interpreter, args: Vec<Value>) -> InterpreterResult<Value> {
        match self {
            Value::NativeFunction { fun, .. } => Ok(fun(interpreter, args)),
            Value::Function { params, body, closure } => {
                let mut env = Some(Rc::new(RefCell::new(LocalEnvironment::new(closure.clone()))));
                std::mem::swap(&mut interpreter.environment, &mut env);

                for ((_, offset), arg) in params.iter().zip(args.into_iter()) {
                    interpreter.environment
                        .as_mut()
                        .unwrap()
                        .borrow_mut()
                        .assign_at(
                            &interpreter.locals[offset],
                            Some(arg)
                        )
                }

                let res = match interpreter.execute_block(&body) {
                    Ok(_) => Ok(Value::Nil),
                    Err(e) => match e.kind {
                        ErrorKind::Return(value) => Ok(value),
                        _ => Err(e)
                    }
                };

                interpreter.environment = env;

                res
            }
            _ => Err(InterpreterError {
                msg: String::new(),
                line: *line,
                kind: ErrorKind::NotCallable,
            })
        }
    }
}

impl Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Number(num) => write!(f, "{}", num),
            Value::String(str) => write!(f, "{}", str),
            Value::Bool(bool) => write!(f, "{}", bool),
            Value::Nil => write!(f, "nil"),
            Value::NativeFunction { .. } => write!(f, "native_func"),
            Value::Function { params, body, ..  } => write!(f, "func({:?}) {{\n{:?}\n}}", params, body),
        }
    }
}


pub struct Interpreter {
    environment: Option<Rc<RefCell<LocalEnvironment>>>,
    globals: GlobalEnvironment,
    pub locals: HashMap<usize, Binding>
}

impl ExprVisitor<InterpreterResult<Value>> for Interpreter {
    #[inline(always)]
    fn visit_expr(&mut self, expr: &Expr) -> InterpreterResult<Value> {
        match expr {
            Expr::Binary { values, operator } => self.visit_binary_expr(&values.0, operator, &values.1),
            Expr::Literal(literal) => match literal {
                Literal::Number(num) => Ok(Value::Number(*num)),
                Literal::String(string) => Ok(Value::String(string.to_string())),
                Literal::Bool(bool) => Ok(Value::Bool(*bool)),
                Literal::Nil => Ok(Value::Nil),
            },
            Expr::Unary { operator, right } => self.visit_unary_expr(operator, right),
            Expr::Ternary { exprs } => self.visit_ternary_expr(&exprs.0, &exprs.1, &exprs.2),
            Expr::Variable { name, line, offset  } => self.visit_var_expr(name, line, offset),
            Expr::Assign { name, value, line, offset  } => self.visit_assign_expr(name, value, line, offset),
            Expr::Logical { values, operator } => self.visit_logical_expr(&values.0, operator, &values.1),
            Expr::Call { line, exprs } => self.visit_call_expr(line, &exprs[0], &exprs[1..]),
            Expr::Lambda { params, body } => self.visit_lambda_expr(params, body),
        }
    }
}

impl StmtVisitor<InterpreterResult<()>> for Interpreter {
    #[inline(always)]
    fn visit_stmt(&mut self, stmt: &Stmt) -> InterpreterResult<()> {
        match stmt {
            Stmt::Expr(expr) => self.visit_expr_stmt(expr),
            Stmt::Print(expr) => self.visit_print_stmt(expr),
            Stmt::Var { name, initializer, offset, .. } => self.visit_var_stmt(name, initializer, *offset),
            Stmt::Block { statements } => self.visit_block_stmt(statements),
            Stmt::If { condition, then_branch, else_branch } => self.visit_if_stmt(condition, then_branch, else_branch),
            Stmt::While { condition, body } => self.visit_while_stmt(condition, body),
            Stmt::Break { line } => self.visit_break_stmt(line),
            Stmt::Function { name, params, body, offset, .. } => self.visit_function_stmt(name, params, body, *offset),
            Stmt::Return { line, value } => self.visit_return_stmt(line, value),
        }
    }
}

impl Interpreter {
    #[inline(always)]
    pub fn new() -> Self {
        Self {
            environment: None,
            globals: GlobalEnvironment::new_global_env(),
            locals: Default::default()
        }
    }
    
    pub fn interpret(&mut self, stmt: &Stmt) -> InterpreterResult<()> {
        let result = self.visit_stmt(stmt);
        if let Err(ref e) = result {
            eprintln!("{}", e);
        }

        result
    }

    #[inline(always)]
    fn visit_assign_expr(&mut self, name: &str, expr: &Expr, line: &usize, offset: &usize) -> InterpreterResult<Value> {
        let value = self.visit_expr(expr)?;

        let status = match self.locals.get(offset) {
            Some(binding) => {
                self.environment.as_mut().unwrap().borrow_mut().assign_at(&binding, Some(value.clone()));
                true
            },
            None => {
                self.globals.assign(name, Some(value.clone()))
            },
        };

        match status {
            true => Ok(value),
            false => Err(InterpreterError{
                msg: name.to_owned(),
                line: *line,
                kind: ErrorKind::UndefinedVariable,
            }),
        }
    }

    #[inline(always)]
    fn visit_logical_expr(&mut self, left: &Expr, operator: &BinaryOp, right: &Expr) -> InterpreterResult<Value> {
        let left = self.visit_expr(left)?;

        if operator.kind == BinaryOpKind::Or {
            if Self::is_truthy(&left) {
                return Ok(left);
            }
        } else if !Self::is_truthy(&left) {
            return Ok(left)
        }

        self.visit_expr(right)
    }

    #[inline(always)]
    fn visit_call_expr(&mut self, line: &usize, callee: &Expr, arguments: &[Expr]) -> InterpreterResult<Value> {
        let callee = self.visit_expr(callee)?;

        let arguments = arguments.into_iter()
            .map(|arg| self.visit_expr(arg))
            .collect::<InterpreterResult<Vec<_>>>()?;

        let arity = callee.arity(line)?;
        if arguments.len() != arity.into() {
            return Err(InterpreterError {
                msg: arity.to_string(),
                line: *line,
                kind: ErrorKind::WrongArity(arguments.len()),
            })
        }

        callee.call(line, self, arguments)
    }

    #[inline(always)]
    fn visit_lambda_expr(&mut self, params: &[(String, usize)], body: &[Stmt]) -> InterpreterResult<Value> {
        Ok(Value::Function {
            params: params.to_vec(),
            body: body.to_vec(),
            closure: self.environment.clone()
        })
    }

    #[inline(always)]
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

    #[inline(always)]
    fn visit_binary_expr(&mut self, left: &Expr, operator: &BinaryOp, right: &Expr) -> InterpreterResult<Value> {
        let (left, right) = (self.visit_expr(left)?, self.visit_expr(right)?);

        match operator.kind {
            BinaryOpKind::Add => match (&left, &right) {
                (Value::Number(left), Value::Number(right)) => Ok(Value::Number(left + right)),
                (Value::String(_), _) | (_, Value::String(_)) => Ok(Value::String(format!("{}{}", left, right))),
                (Value::Number(_), v) => Self::invalid_operand(&v, "number or string", operator.line),
                (Value::Bool(_), v) | (Value::Nil, v) => Self::invalid_operand(&v, "string", operator.line),
                (v, _) => Self::invalid_operand(&v, "number or string", operator.line)
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
            _ => unreachable!()
        }
    }

    #[inline(always)]
    fn visit_ternary_expr(&mut self, condition: &Expr, then_branch: &Expr, else_branch: &Expr) -> InterpreterResult<Value> {
        let result = self.visit_expr(condition)?;
        self.visit_expr(match Self::is_truthy(&result) {
            true => then_branch,
            false => else_branch,
        })
    }

    #[inline(always)]
    fn visit_var_expr(&mut self, name: &str, line: &usize, offset: &usize) -> InterpreterResult<Value> {
        let value = match self.locals.get(offset) {
            Some(binding) => self.environment.as_ref().unwrap().borrow().get_at(binding),
            None => self.globals.get(name),
        };

        match value {
            Some(Some(value)) => Ok(value),
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

    #[inline(always)]
    fn visit_expr_stmt(&mut self, expr: &Expr) -> InterpreterResult<()> {
        self.visit_expr(expr)?;
        Ok(())
    }

    #[inline(always)]
    fn visit_print_stmt(&mut self, expr: &Expr) -> InterpreterResult<()> {
        let value = self.visit_expr(expr)?;
        println!("{}", value);
        Ok(())
    }
    
    #[inline(always)]
    fn visit_var_stmt(&mut self, name: &str, initializer: &Option<Expr>, offset: usize) -> InterpreterResult<()> {
        let value = if let Some(expr) = initializer {
            Some(self.visit_expr(expr)?)
        } else {
            None
        };

        match self.locals.get(&offset) {
            Some(binding) => self.environment
                .as_ref()
                .unwrap()
                .borrow_mut()
                .assign_at(
                    binding,
                    value
                ),
            None => {self.globals.define(name, value);},
        };
        
        Ok(())
    }
    
    #[inline(always)]
    fn visit_block_stmt(&mut self, statements: &[Stmt]) -> InterpreterResult<()> {
        self.create_new_env();

        let res = self.execute_block(statements);

        self.remove_new_env();

        res
    }

    #[inline(always)]
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
    fn visit_while_stmt(&mut self, condition: &Expr, body: &Stmt) -> InterpreterResult<()> {
        match body {
            Stmt::Block { statements } => {
                self.create_new_env();

                while Self::is_truthy(&self.visit_expr(condition)?) {
                    for stmt in statements {
                        let result = self.visit_stmt(stmt);
                        if result.is_err() {
                            self.remove_new_env();
                            return result;
                        }
                    }
                    
                    self.environment.as_mut().unwrap().borrow_mut().clear()
                }
                
                self.remove_new_env();
            },
            _ => {
                while Self::is_truthy(&self.visit_expr(condition)?) {
                    self.visit_stmt(body)?
                }
            }
        }

        Ok(())
    }

    #[inline(always)]
    fn visit_break_stmt(&mut self, line: &usize) -> InterpreterResult<()> {
        Err(InterpreterError {
            msg: String::new(),
            line: *line,
            kind: ErrorKind::Break,
        })
    }

    fn visit_function_stmt(&mut self, name: &str, params: &[(String, usize)], body: &[Stmt], offset: usize) -> InterpreterResult<()> {
        let func = Some(Value::Function {
            params: params.to_vec(),
            body: body.to_vec(),
            closure: self.environment.clone()
        });

        match self.locals.get(&offset) {
            Some(binding) => self.environment
                .as_mut()
                .unwrap()
                .borrow_mut()
                .assign_at(
                    binding,
                    func
                ),
            None => {self.globals.define(name, func);},
        };

        Ok(())
    }

    fn visit_return_stmt(&mut self, line: &usize, value: &Expr) -> InterpreterResult<()> {
        Err(InterpreterError{
            msg: String::new(),
            line: *line,
            kind: ErrorKind::Return(
                if value != &Expr::Literal(Literal::Nil) {
                    self.visit_expr(value)?
                } else {
                    Value::Nil
                }
            ),
        })
    }

    #[inline(always)]
    fn is_truthy(value: &Value) -> bool {
        match value {
            Value::String(string) => string.len() > 0,
            Value::Bool(bool) => *bool,
            Value::Number(num) => *num != 0.0,
            Value::Nil => false,
            Value::NativeFunction { .. } => true,
            Value::Function { .. } => true,
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
            Value::NativeFunction { .. } => "native_func".to_owned(),
            Value::Function { .. } => "func".to_owned(),
        };
        Err(
            InterpreterError {
                msg,
                line,
                kind: ErrorKind::InvalidOperand(expected),
            }
        )
    }

    #[inline(always)]
    fn execute_block(&mut self, statements: &[Stmt]) -> InterpreterResult<()> {
        for stmt in statements {
            self.visit_stmt(stmt)?;
        }

        Ok(())
    }

    #[inline(always)]
    fn create_new_env(&mut self) {
        let enclosing = self.environment.clone();
        self.environment = Some(Rc::new(RefCell::new(LocalEnvironment::new(enclosing))));
    }

    #[inline(always)]
    fn remove_new_env(&mut self) {
        let mut env = self.environment.as_mut().unwrap().borrow_mut();
        let enclosing = env.enclosing.take();
        drop(env);
        self.environment = enclosing;
    }

    #[inline(always)]
    pub fn resolve(&mut self, offset: usize, binding: Binding) {
        self.locals.insert(offset, binding);
    }
}