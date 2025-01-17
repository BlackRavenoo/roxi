use std::{cell::RefCell, collections::HashMap, fmt::Display, rc::Rc};

use crate::{environment::{GlobalEnvironment, LocalEnvironment}, expr::{BinaryOp, BinaryOpKind, Expr, ExprVisitor, Literal, UnaryOp, UnaryOpKind}, resolver::Binding, stmt::{Stmt, StmtVisitor}};

#[derive(Debug)]
enum ErrorKind {
    InvalidOperand(&'static str),
    DivisionByZero,
    UninitializedVariable,
    UndefinedVariable,
    NotCallable,
    PropertyOnNonInstance(Rc<RefCell<Value>>),
    UndefinedProperty,
    SuperclassIsNotClass,
    WrongArity(usize),
    Return(Rc<RefCell<Value>>),
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
            ErrorKind::PropertyOnNonInstance(ref val) => write!(
                f,
                "[line {}] Error: Tried to access field `{}` on {}.",
                self.line,
                self.msg,
                val.borrow()
            ),
            ErrorKind::UndefinedProperty => write!(
                f,
                "[line {}] Error: Undefined property `{}`.",
                self.line,
                self.msg
            ),
            ErrorKind::SuperclassIsNotClass => write!(
                f,
                "[line {}] Error: Superclass is not a class.",
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
        fun: fn(&mut Interpreter, Vec<Rc<RefCell<Value>>>) -> Value
    },
    Function {
        params: Vec<usize>,
        body: Vec<Stmt>,
        closure: Option<Rc<RefCell<LocalEnvironment>>>,
        is_initializer: bool
    },
    Class(Rc<RefCell<Class>>),
    Instance {
        class: Rc<RefCell<Class>>,
        fields: HashMap<String, Rc<RefCell<Value>>>
    },
    Nil,
}

#[derive(Clone, Debug)]
pub struct Class {
    name: String,
    superclass: Option<Rc<RefCell<Class>>>,
    methods: HashMap<String, Rc<RefCell<Value>>>,
    static_methods: HashMap<String, Rc<RefCell<Value>>>,
    getters: HashMap<String, Rc<RefCell<Value>>>
}

impl Class {
    fn find_method(&self, name: &str) -> Option<Rc<RefCell<Value>>> {
        self.methods.get(name).cloned()
            .or_else(|| {
                self.superclass
                    .as_ref()
                    .and_then(|superclass| superclass.borrow().find_method(name))
            })
    }

    fn find_static_method(&self, name: &str) -> Option<Rc<RefCell<Value>>> {
        self.static_methods.get(name).cloned()
            .or_else(|| {
                self.superclass
                    .as_ref()
                    .and_then(|superclass| superclass.borrow().find_static_method(name))
            })
    }

    fn find_getter(&self, name: &str) -> Option<Rc<RefCell<Value>>> {
        self.getters.get(name).cloned()
            .or_else(|| {
                self.superclass
                    .as_ref()
                    .and_then(|superclass| superclass.borrow().find_getter(name))
            })
    }
}

impl Display for Class {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.name)
    }
}

impl Value {
    pub fn arity(&self, line: &usize) -> InterpreterResult<u16> {
        match self {
            Value::NativeFunction { arity,.. } => Ok(*arity),
            Value::Function { params, .. } => Ok(params.len() as u16),
            Value::Class(class) => if let Some(initializer) = class.borrow().find_method("init") {
                initializer.borrow().arity(line)
            } else {
                Ok(0)
            },
            _ => Err(InterpreterError {
                msg: String::new(),
                line: *line,
                kind: ErrorKind::NotCallable,
            })
        }
    }

    pub fn call(&self, line: &usize, interpreter: &mut Interpreter, args: Vec<Rc<RefCell<Value>>>) -> InterpreterResult<Rc<RefCell<Value>>> {
        match self {
            Value::NativeFunction { fun, .. } => Ok(Rc::new(RefCell::new(fun(interpreter, args)))),
            Value::Function { params, body, closure, is_initializer } => {
                let mut env = Some(Rc::new(RefCell::new(LocalEnvironment::new(closure.clone()))));
                std::mem::swap(&mut interpreter.environment, &mut env);

                for (offset, arg) in params.iter().zip(args.into_iter()) {
                    interpreter.environment
                        .as_mut()
                        .unwrap()
                        .borrow_mut()
                        .assign_at(
                            &interpreter.locals[offset],
                            Some(arg)
                        )
                }

                let res = match interpreter.execute_block(body) {
                    Ok(_) => Ok(Rc::new(RefCell::new(Value::Nil))),
                    Err(e) => match e.kind {
                        ErrorKind::Return(value) => Ok(value),
                        _ => Err(e)
                    }
                };

                
                let res = if *is_initializer {
                    Ok(interpreter.environment
                        .as_mut()
                        .unwrap()
                        .borrow_mut()
                        .get_at(
                            &Binding { scopes_up: 1, index: 0 }
                        )
                        .unwrap()
                        .unwrap()
                    )
                } else {
                    res
                };

                interpreter.environment = env;

                res
            },
            Value::Class(class) => {
                let instance = Rc::new(RefCell::new(
                    Value::Instance {
                        class: Rc::clone(class),
                        fields: HashMap::new()
                    }
                ));

                if let Some(initializer) = class.borrow().find_method("init") {
                    if let Value::Function {
                        closure,
                        params,
                        body,
                        is_initializer 
                    } = &*initializer.borrow() {
                        let mut closure = LocalEnvironment::new(closure.clone());
                        closure.assign(0, Some(instance.clone()));
                        let func = Value::Function {
                            params: params.to_vec(),
                            body: body.to_vec(),
                            closure: Some(Rc::new(RefCell::new(closure))),
                            is_initializer: *is_initializer
                        };
                        func.call(line, interpreter, args)?;
                    }
                }

                Ok(instance)
            },
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
            Value::Function { ..  } => write!(f, "func"),
            Value::Class(class) => write!(f, "{}", class.borrow()),
            Value::Instance { class, .. } => write!(f, "{} instance", class.borrow()),
        }
    }
}

pub struct Interpreter {
    environment: Option<Rc<RefCell<LocalEnvironment>>>,
    globals: GlobalEnvironment,
    pub locals: HashMap<usize, Binding>
}

impl ExprVisitor<InterpreterResult<Rc<RefCell<Value>>>> for Interpreter {
    #[inline(always)]
    fn visit_expr(&mut self, expr: &Expr) -> InterpreterResult<Rc<RefCell<Value>>> {
        match expr {
            Expr::Binary { values, operator } => self.visit_binary_expr(&values.0, operator, &values.1),
            Expr::Literal(literal) => match literal {
                Literal::Number(num) => Ok(Rc::new(RefCell::new(Value::Number(*num)))),
                Literal::String(string) => Ok(Rc::new(RefCell::new(Value::String(string.to_string())))),
                Literal::Bool(bool) => Ok(Rc::new(RefCell::new(Value::Bool(*bool)))),
                Literal::Nil => Ok(Rc::new(RefCell::new(Value::Nil))),
            },
            Expr::Unary { operator, right } => self.visit_unary_expr(operator, right),
            Expr::Ternary { exprs } => self.visit_ternary_expr(&exprs.0, &exprs.1, &exprs.2),
            Expr::Variable { name, line, offset  } => self.visit_var_expr(name, line, offset),
            Expr::Assign { name, value, line, offset  } => self.visit_assign_expr(name, value, line, offset),
            Expr::Logical { values, operator } => self.visit_logical_expr(&values.0, operator, &values.1),
            Expr::Call { line, exprs } => self.visit_call_expr(line, &exprs[0], &exprs[1..]),
            Expr::Lambda { params, body } => self.visit_lambda_expr(params, body),
            Expr::Get { name, object, line } => self.visit_get_expr(name, object, *line),
            Expr::Set { name, object, value, line } => self.visit_set_expr(name, object, value, *line),
            Expr::This { line, offset } => self.visit_this_expr(*line, offset),
            Expr::Super { method, offset, line } => self.visit_super_expr(method, offset, *line),
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
            Stmt::Class { name, superclass, methods, offset, static_methods, getters, .. } => self.visit_class_stmt(name, superclass, methods, static_methods, getters, offset),
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
    fn visit_assign_expr(&mut self, name: &str, expr: &Expr, line: &usize, offset: &usize) -> InterpreterResult<Rc<RefCell<Value>>> {
        let value = self.visit_expr(expr)?;

        let status = match self.locals.get(offset) {
            Some(binding) => {
                self.environment.as_mut().unwrap().borrow_mut().assign_at(binding, Some(value.clone()));
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
    fn visit_logical_expr(&mut self, left: &Expr, operator: &BinaryOp, right: &Expr) -> InterpreterResult<Rc<RefCell<Value>>> {
        let left = self.visit_expr(left)?;

        if operator.kind == BinaryOpKind::Or {
            if Self::is_truthy(&left.borrow()) {
                return Ok(left);
            }
        } else if !Self::is_truthy(&left.borrow()) {
            return Ok(left)
        }

        self.visit_expr(right)
    }

    #[inline(always)]
    fn visit_call_expr(&mut self, line: &usize, callee: &Expr, arguments: &[Expr]) -> InterpreterResult<Rc<RefCell<Value>>> {
        let callee = self.visit_expr(callee)?;

        let arguments = arguments.iter()
            .map(|arg| self.visit_expr(arg))
            .collect::<InterpreterResult<Vec<_>>>()?;

        let arity = callee.borrow().arity(line)?;
        if arguments.len() != arity.into() {
            return Err(InterpreterError {
                msg: arity.to_string(),
                line: *line,
                kind: ErrorKind::WrongArity(arguments.len()),
            })
        }

        let x = callee.borrow().call(line, self, arguments);
        x
    }

    #[inline(always)]
    fn visit_lambda_expr(&mut self, params: &[(String, usize)], body: &[Stmt]) -> InterpreterResult<Rc<RefCell<Value>>> {
        Ok(Rc::new(RefCell::new(Value::Function {
            params: params.iter().map(|x| x.1).collect(),
            body: body.to_vec(),
            closure: self.environment.clone(),
            is_initializer: false
        })))
    }

    #[inline(always)]
    fn visit_get_expr(&mut self, name: &str, object: &Expr, line: usize) -> InterpreterResult<Rc<RefCell<Value>>> {
        let object = self.visit_expr(object)?;
        match &*object.clone().borrow() {
            Value::Instance { fields, class } => {
                if let Some(val) = fields.get(name) {
                    Ok(val.clone())
                } else if let Some(method) = class.borrow().find_method(name) {
                    match &*method.borrow() {
                        Value::Function { params, body, closure, is_initializer } => {
                            let mut closure = LocalEnvironment::new(closure.clone());
                            closure.assign(0, Some(object));
                            Ok(Rc::new(RefCell::new(Value::Function {
                                params: params.to_vec(),
                                body: body.to_vec(),
                                closure: Some(Rc::new(RefCell::new(closure))),
                                is_initializer: *is_initializer
                            })))
                        },
                        _ => unreachable!(),
                    }
                } else if let Some(getter) = class.borrow().find_getter(name) {
                    match &*getter.borrow() {
                        Value::Function {
                            params,
                            body,
                            closure,
                            is_initializer
                        } => {
                            let mut closure = LocalEnvironment::new(closure.clone());
                            closure.assign(0, Some(object));
                            Value::Function {
                                params: params.to_vec(),
                                body: body.to_vec(),
                                closure: Some(Rc::new(RefCell::new(closure))),
                                is_initializer: *is_initializer
                            }.call(&line, self, Vec::new())
                        },
                        _ => unreachable!()
                    } 
                } else {
                    Err(InterpreterError {
                        msg: name.to_owned(),
                        line,
                        kind: ErrorKind::UndefinedProperty,
                    })
                }
            }
            Value::Class(class) => {
                match class.borrow().find_static_method(name) {
                    Some(method) => Ok(method.clone()),
                    None => Err(InterpreterError {
                        msg: name.to_owned(),
                        line,
                        kind: ErrorKind::UndefinedProperty,
                    }),
                }
            }
            _ => Err(InterpreterError {
                msg: name.to_owned(),
                line,
                kind: ErrorKind::PropertyOnNonInstance(object),
            })
        }
    }

    #[inline(always)]
    fn visit_set_expr(&mut self, name: &str, object: &Expr, value: &Expr, line: usize) -> InterpreterResult<Rc<RefCell<Value>>> {
        let object_rc = self.visit_expr(object)?;

        match &*(object_rc.borrow()) {
            Value::Instance { .. } => (),
            _ => return Err(InterpreterError{
                msg: name.to_owned(),
                line,
                kind: ErrorKind::PropertyOnNonInstance(object_rc.clone()),
            })
        }
        
        let value = self.visit_expr(value)?;
        let mut object = object_rc.borrow_mut();
        
        match &mut *object {
            Value::Instance { fields, .. } => {
                fields.insert(name.to_owned(), value);
                Ok(object_rc.clone())
            }
            _ => unreachable!()
        }
    }
    #[inline(always)]
    fn visit_this_expr(&mut self, line: usize, offset: &usize) -> InterpreterResult<Rc<RefCell<Value>>> {
        let value = match self.locals.get(offset) {
            Some(binding) => self.environment.as_ref().unwrap().borrow().get_at(binding),
            None => unreachable!(),
        };

        match value {
            Some(Some(value)) => Ok(value),
            Some(None) => Err(InterpreterError{
                msg: "this".to_owned(),
                line,
                kind: ErrorKind::UninitializedVariable,
            }),
            None => Err(InterpreterError{
                msg: "this".to_owned(),
                line,
                kind: ErrorKind::UndefinedVariable,
            }),
        }
    }

    #[inline(always)]
    fn visit_super_expr(&mut self, method: &str, offset: &usize, line: usize) -> InterpreterResult<Rc<RefCell<Value>>> {
        let binding = &self.locals[offset];

        let env = self.environment.as_ref().unwrap().borrow();
        
        let superclass = env
            .get_at(binding)
            .unwrap()
            .unwrap();

        let this = env
            .get_at(&Binding { scopes_up: binding.scopes_up - 1, index: 0 })
            .unwrap()
            .unwrap();

        drop(env);
        
        let method = match &*superclass.borrow() {
            Value::Class (class) => match class.borrow().find_method(method) {
                Some(method) => match &*method.borrow() {
                    Value::Function { params, body, closure, is_initializer } => {
                        let mut closure = LocalEnvironment::new(closure.clone());
                        closure.assign(0, Some(this));
                        Ok(Rc::new(RefCell::new(Value::Function {
                            params: params.to_vec(),
                            body: body.to_vec(),
                            closure: Some(Rc::new(RefCell::new(closure))),
                            is_initializer: *is_initializer,
                        })))
                    },
                    _ => unreachable!()
                },
                None => match class.borrow().find_getter(method) {
                    Some(getter) => {
                        match &*getter.borrow() {
                            Value::Function { params, body, closure, is_initializer } => {
                                let mut closure = LocalEnvironment::new(closure.clone());
                                closure.assign(0, Some(this));
                                Value::Function {
                                    params: params.to_vec(),
                                    body: body.to_vec(),
                                    closure: Some(Rc::new(RefCell::new(closure))),
                                    is_initializer: *is_initializer,
                                }.call(&line, self, Vec::new())
                            },
                            _ => unreachable!()
                        }
                    },
                    None => return Err(InterpreterError {
                        msg: method.to_owned(),
                        line,
                        kind: ErrorKind::UndefinedProperty,
                    }),
                },
            },
            _ => unreachable!()
        };

        method
    }

    #[inline(always)]
    fn visit_unary_expr(&mut self, operator: &UnaryOp, right: &Expr) -> InterpreterResult<Rc<RefCell<Value>>> {
        let right = self.visit_expr(right)?;
        let right = &*right.borrow();
        
        match operator.kind {
            UnaryOpKind::Neg => {
                match right {
                    Value::Number(num) => Ok(Rc::new(RefCell::new(Value::Number(-num)))),
                    v => Self::invalid_operand(v, "number", operator.line)
                }
            },
            UnaryOpKind::Not => Ok(Rc::new(RefCell::new(Value::Bool(!Self::is_truthy(right))))),
        }
    }

    #[inline(always)]
    fn visit_binary_expr(&mut self, left: &Expr, operator: &BinaryOp, right: &Expr) -> InterpreterResult<Rc<RefCell<Value>>> {
        let left_rc = self.visit_expr(left)?;
        let right_rc = self.visit_expr(right)?;
        let (left, right) = (&*left_rc.borrow(), &*right_rc.borrow());

        match operator.kind {
            BinaryOpKind::Add => match (&left, &right) {
                (Value::Number(left), Value::Number(right)) => Ok(Rc::new(RefCell::new(Value::Number(left + right)))),
                (Value::String(_), _) | (_, Value::String(_)) => Ok(Rc::new(RefCell::new(Value::String(format!("{}{}", left, right))))),
                (Value::Number(_), v) => Self::invalid_operand(v, "number or string", operator.line),
                (Value::Bool(_), v) | (Value::Nil, v) => Self::invalid_operand(v, "string", operator.line),
                (v, _) => Self::invalid_operand(v, "number or string", operator.line)
            },
            BinaryOpKind::Sub => match (left, right) {
                (Value::Number(left), Value::Number(right)) => Ok(Rc::new(RefCell::new(Value::Number(left - right)))),
                (Value::Number(_), v) => Self::invalid_operand(v, "number", operator.line),
                (v, _) => Self::invalid_operand(v, "number", operator.line)
            },
            BinaryOpKind::Mul => match (left, right) {
                (Value::Number(left), Value::Number(right)) => Ok(Rc::new(RefCell::new(Value::Number(left * right)))),
                (Value::Number(_), v) => Self::invalid_operand(v, "number", operator.line),
                (v, _) => Self::invalid_operand(v, "number", operator.line)
            },
            BinaryOpKind::Div => match (left, right) {
                (Value::Number(_), Value::Number(right)) if *right == 0.0 => Err(
                    InterpreterError{
                        msg: String::new(),
                        line: operator.line,
                        kind: ErrorKind::DivisionByZero,
                    }
                ),
                (Value::Number(left), Value::Number(right)) => Ok(Rc::new(RefCell::new(Value::Number(left / right)))),
                (Value::Number(_), v) => Self::invalid_operand(v, "number", operator.line),
                (v, _) => Self::invalid_operand(v, "number", operator.line)
            },
            BinaryOpKind::Eq => Ok(Rc::new(RefCell::new(Value::Bool(Self::is_equal(left, right))))),
            BinaryOpKind::Ne => Ok(Rc::new(RefCell::new(Value::Bool(!Self::is_equal(left, right))))),
            BinaryOpKind::Lt => match (left, right) {
                (Value::Number(left), Value::Number(right)) => Ok(Rc::new(RefCell::new(Value::Bool(left < right)))),
                (Value::Number(_), v) => Self::invalid_operand(v, "number", operator.line),
                (v, _) => Self::invalid_operand(v, "number", operator.line)
            },
            BinaryOpKind::Le => match (left, right) {
                (Value::Number(left), Value::Number(right)) => Ok(Rc::new(RefCell::new(Value::Bool(left <= right)))),
                (Value::Number(_), v) => Self::invalid_operand(v, "number", operator.line),
                (v, _) => Self::invalid_operand(v, "number", operator.line)
            },
            BinaryOpKind::Gt => match (left, right) {
                (Value::Number(left), Value::Number(right)) => Ok(Rc::new(RefCell::new(Value::Bool(left > right)))),
                (Value::Number(_), v) => Self::invalid_operand(v, "number", operator.line),
                (v, _) => Self::invalid_operand(v, "number", operator.line)
            },
            BinaryOpKind::Ge => match (left, right) {
                (Value::Number(left), Value::Number(right)) => Ok(Rc::new(RefCell::new(Value::Bool(left >= right)))),
                (Value::Number(_), v) => Self::invalid_operand(v, "number", operator.line),
                (v, _) => Self::invalid_operand(v, "number", operator.line)
            },
            _ => unreachable!()
        }
    }

    #[inline(always)]
    fn visit_ternary_expr(&mut self, condition: &Expr, then_branch: &Expr, else_branch: &Expr) -> InterpreterResult<Rc<RefCell<Value>>> {
        let result = self.visit_expr(condition)?;
        let res = self.visit_expr(match Self::is_truthy(&result.borrow()) {
            true => then_branch,
            false => else_branch,
        });
        
        res
    }

    #[inline(always)]
    fn visit_var_expr(&mut self, name: &str, line: &usize, offset: &usize) -> InterpreterResult<Rc<RefCell<Value>>> {
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
        println!("{}", value.borrow());
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
        if Self::is_truthy(&self.visit_expr(condition)?.borrow()) {
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

                while Self::is_truthy(&self.visit_expr(condition)?.borrow()) {
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
                while Self::is_truthy(&self.visit_expr(condition)?.borrow()) {
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
        let func = Some(Rc::new(RefCell::new(Value::Function {
            params: params.iter().map(|x| x.1).collect(),
            body: body.to_vec(),
            closure: self.environment.clone(),
            is_initializer: false
        })));

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

    #[inline(always)]
    fn visit_return_stmt(&mut self, line: &usize, value: &Expr) -> InterpreterResult<()> {
        Err(InterpreterError{
            msg: String::new(),
            line: *line,
            kind: ErrorKind::Return(
                if value != &Expr::Literal(Literal::Nil) {
                    self.visit_expr(value)?
                } else {
                    Rc::new(RefCell::new(Value::Nil))
                }
            ),
        })
    }

    fn visit_class_stmt(&mut self, name: &str, superclass: &Option<Expr>, methods: &[Stmt], static_methods: &[Stmt], getters: &[Stmt], offset: &usize) -> InterpreterResult<()> {
        let superclass = if let Some(superclass) = superclass {
            let value_rc = self.visit_expr(superclass)?;
            let value = &*value_rc.borrow();
            let class = match value {
                Value::Class(class) => class,
                _ => {
                    let line = match superclass {
                        Expr::Variable { line, .. } => *line,
                        _ => unreachable!()
                    };

                    return Err(InterpreterError{
                        msg: String::new(),
                        line,
                        kind: ErrorKind::SuperclassIsNotClass,
                    })
                }
            };

            self.create_new_env();
            self.environment.as_ref().unwrap().borrow_mut().assign(0 , Some(Rc::new(RefCell::new(Value::Class(class.clone())))));
            Some(class.clone())
        } else {
            None
        };

        
        let mut class = Class {
            name: name.to_owned(),
            superclass,
            methods: HashMap::with_capacity(methods.len()),
            static_methods: HashMap::with_capacity(static_methods.len()),
            getters: HashMap::with_capacity(getters.len())
        };

        for method in methods.iter() {
            if let Stmt::Function { name, params, body, .. } = method {
                class.methods.insert(
                    name.to_owned(),
                    Rc::new(RefCell::new(Value::Function{
                        params: params.iter().map(|x| x.1).collect(),
                        body: body.to_vec(),
                        closure: self.environment.clone(),
                        is_initializer: name == "init"
                    }))
                );
            }
        }

        for static_method in static_methods {
            if let Stmt::Function { name, params, body, .. } = static_method {
                class.static_methods.insert(
                    name.to_owned(),
                    Rc::new(RefCell::new(Value::Function{
                        params: params.iter().map(|x| x.1).collect(),
                        body: body.to_vec(),
                        closure: self.environment.clone(),
                        is_initializer: false
                    }))
                );
            }
        }

        for getter in getters {
            if let Stmt::Function { name, params, body, .. } = getter {
                class.getters.insert(
                    name.to_owned(),
                    Rc::new(RefCell::new(Value::Function{
                        params: params.iter().map(|x| x.1).collect(),
                        body: body.to_vec(),
                        closure: self.environment.clone(),
                        is_initializer: false
                    }))
                );
            }
        }
        
        if class.superclass.is_some() {
            self.remove_new_env();
        };
        
        let class = Some(
            Rc::new(RefCell::new(
                Value::Class(
                    Rc::new(RefCell::new(class))
                )
            ))
        );

        match self.locals.get(offset) {
            Some(binding) => self.environment
                .as_ref()
                .unwrap()
                .borrow_mut()
                .assign_at(
                    binding,
                    class
                ),
            None => self.globals.define(name, class)
        };

        Ok(())
    }

    #[inline(always)]
    fn is_truthy(value: &Value) -> bool {
        match value {
            Value::String(string) => !string.is_empty(),
            Value::Bool(bool) => *bool,
            Value::Number(num) => *num != 0.0,
            Value::Nil => false,
            _ => true
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

    fn invalid_operand(found: &Value, expected: &'static str, line: usize) -> InterpreterResult<Rc<RefCell<Value>>> {
        let msg = match found {
            Value::Number(num) => format!("number: {}", num),
            Value::String(str) => format!("string: \"{}\"", str),
            Value::Bool(bool) => format!("bool: {}", bool),
            Value::Nil => String::from("nil"),
            Value::NativeFunction { .. } => "native_func".to_owned(),
            Value::Function { .. } => "func".to_owned(),
            Value::Class(class) => format!("{}", class.borrow()),
            Value::Instance { class, .. } => format!("{} instance", class.borrow()),
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