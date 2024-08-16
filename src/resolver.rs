use std::{collections::HashMap, fmt::Display};

use wyhash2::WyHash;

use crate::{expr::{Expr, ExprVisitor}, interpreter::Interpreter, stmt::{Stmt, StmtVisitor}};

#[derive(Debug)]
pub enum ResolverError {
    SelfRefInitializer {
        name: String,
        line: usize
    },
    ReDeclaration {
        name: String,
        line: usize
    },
    ReturnOutsideFunction {
        line: usize
    }
}

impl Display for ResolverError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ResolverError::SelfRefInitializer { name, line } => write!(
                f,
                "[line {}] Error: Can't read local variable `{}` in its own initializer.",
                line,
                name
            ),
            ResolverError::ReDeclaration { name, line } => write!(
                f,
                "[line {}] Error: Variable `{}` already exists in this scope.",
                line,
                name
            ),
            ResolverError::ReturnOutsideFunction { line } => write!(
                f,
                "[line {}] Error: `return` outside function.",
                line
            )
        }
    }
}

pub type ResolverResult<T> = Result<T, ResolverError>;

#[derive(PartialEq)]
enum FunctionKind {
    None,
    Function
}

#[derive(Debug)]
pub struct Binding {
    pub scopes_up: usize,
    pub index: usize
}

impl Binding {
    #[inline(always)]
    pub fn new(scopes_up: usize, index: usize) -> Self {
        Self {
            scopes_up,
            index
        }
    }
}

#[derive(PartialEq, Debug)]
enum VariableState {
    Declared,
    Defined,
    Used
}

#[derive(Debug)]
struct VariableData {
    state: VariableState,
    index: usize
}

impl VariableData {
    #[inline(always)]
    fn new(state: VariableState, index: usize) -> Self {
        Self {
            state,
            index
        }
    }
}

pub struct Resolver<'a> {
    interpreter: &'a mut Interpreter,
    scopes: Vec<HashMap<String, VariableData, WyHash>>,
    current_function: FunctionKind
}

impl StmtVisitor<ResolverResult<()>> for Resolver<'_> {
    #[inline(always)]
    fn visit_stmt(&mut self, stmt: &Stmt) -> ResolverResult<()> {
        match stmt {
            Stmt::Expr(expr) => self.visit_expr(expr),
            Stmt::Print(expr) => self.visit_expr(expr),
            Stmt::Return { value, line } => self.visit_return_stmt(value, line),
            Stmt::Var { name, initializer, line, offset } => self.visit_var_stmt(name, initializer, line, *offset),
            Stmt::Block { statements } => self.visit_block_stmt(statements),
            Stmt::Function { name, params, body, line, offset } => self.visit_function_stmt(name, params, body, *line, *offset),
            Stmt::If { condition, then_branch, else_branch } => self.visit_if_stmt(condition, then_branch, else_branch),
            Stmt::While { condition, body } => self.visit_while_stmt(condition, body),
            Stmt::Break { .. } => Ok(()),
            Stmt::Class { name, line, offset, .. } => self.visit_class_stmt(name, line, offset),
        }
    }
}

impl ExprVisitor<ResolverResult<()>> for Resolver<'_> {
    #[inline(always)]
    fn visit_expr(&mut self, expr: &Expr) -> ResolverResult<()> {
        match expr {
            Expr::Assign { name, value, offset, .. } => self.visit_assign_expr(name, value, offset),
            Expr::Binary { values, .. } => self.visit_binary_expr(&values.0, &values.1),
            Expr::Call { exprs, .. } => self.visit_call_expr(&exprs[0], &exprs[1..]),
            Expr::Literal(_) => Ok(()),
            Expr::Unary { right, .. } => self.visit_expr(right),
            Expr::Ternary { exprs } => self.visit_ternary_expr(&exprs.0, &exprs.1, &exprs.2),
            Expr::Variable { name, line, offset } => self.visit_variable_expr(name, line, offset),
            Expr::Logical { values, .. } => self.visit_logical_expr(&values.0, &values.1),
            Expr::Lambda { params, body } => self.resolve_function(params, body, 0, FunctionKind::Function),
            Expr::Get { object, .. } => self.visit_expr(object),
            Expr::Set { object, value, ..} => { self.visit_expr(object)?; self.visit_expr(value) },
        }
    }
}

impl Resolver<'_> {
    #[inline(always)]
    pub fn new(interpreter: &mut Interpreter) -> Resolver {
        Resolver {
            interpreter,
            scopes: Vec::with_capacity(2),
            current_function: FunctionKind::None
        }
    }

    pub fn resolve_stmts(&mut self, stmts: &[Stmt]) -> ResolverResult<()> {
        for stmt in stmts.iter() {
            self.visit_stmt(stmt)?
        };

        Ok(())
    }

    fn resolve_local(&mut self, name: &str, offset: usize) {
        let scopes_len = self.scopes.len();
        for (i, scope) in self.scopes.iter_mut().enumerate().rev() {
            if let Some(var) = scope.get_mut(name) {
                var.state = VariableState::Used;
                self.interpreter.resolve(offset, Binding::new(scopes_len - 1 - i, var.index));
                return;
            }
        }
    }
    
    fn begin_scope(&mut self) {
        self.scopes.push(HashMap::with_hasher(WyHash::with_seed(0)))
    }
    
    fn end_scope(&mut self) {
        if let Some(scope) = self.scopes.pop() {
            for (name, var) in scope {
                if var.state != VariableState::Used {
                    eprintln!("Warning: Unused local variable `{}`.", name)
                }
            }
        }
    }

    fn declare(&mut self, name: &str, line: usize) -> ResolverResult<()> {
        if let Some(scope) = self.scopes.last_mut() {
            if scope.contains_key(name) {
                return Err(ResolverError::ReDeclaration { name: name.to_string(), line })
            }
            scope.insert(name.to_string(), VariableData::new(VariableState::Declared, scope.len()));
        }
        Ok(())
    }

    fn define(&mut self, name: &str) {
        if let Some(scope) = self.scopes.last_mut() {
            if let Some(var) = scope.get_mut(name) {
                var.state = VariableState::Defined
            }
        }
    }

    #[inline(always)]
    fn visit_return_stmt(&mut self, expr: &Expr, line: &usize) -> ResolverResult<()> {
        if self.current_function == FunctionKind::None {
            return Err(ResolverError::ReturnOutsideFunction { line: *line })
        }
        self.visit_expr(expr)
    }
    
    #[inline(always)]
    fn visit_var_stmt(&mut self, name: &str, initializer: &Option<Expr>, line: &usize, offset: usize) -> ResolverResult<()>  {
        self.declare(name, *line)?;
        let res = if let Some(initializer) = initializer {
            self.visit_expr(initializer)
        } else {
            Ok(())
        };
        self.define(name);
        self.resolve_local(name, offset);
        res
    }

    #[inline(always)]
    fn visit_block_stmt(&mut self, statements: &[Stmt]) -> ResolverResult<()> {
        self.begin_scope();
        self.resolve_stmts(statements)?;
        self.end_scope();
        Ok(())
    }

    #[inline(always)]
    fn visit_function_stmt(&mut self, name: &str, params: &[(String, usize)], body: &[Stmt], line: usize, offset: usize) -> ResolverResult<()> {
        self.declare(name, line)?;
        self.define(name);
        self.resolve_function(params, body, line, FunctionKind::Function)?;
        self.resolve_local(name, offset);
        Ok(())
    }

    #[inline(always)]
    fn visit_if_stmt(&mut self, condition: &Expr, then_branch: &Stmt, else_branch: &Option<Box<Stmt>>) -> ResolverResult<()> {
        self.visit_expr(condition)?;
        self.visit_stmt(then_branch)?;
        if let Some(else_branch) = else_branch {
            self.visit_stmt(else_branch)?;
        }

        Ok(())
    }

    #[inline(always)]
    fn visit_while_stmt(&mut self, condition: &Expr, body: &Stmt) -> ResolverResult<()> {
        self.visit_expr(condition)?;
        self.visit_stmt(body)
    }

    #[inline(always)]
    fn visit_class_stmt(&mut self, name: &str, line: &usize, offset: &usize) -> ResolverResult<()> {
        self.declare(name, *line)?;
        self.define(name);
        self.resolve_local(name, *offset);
        Ok(())
    }

    #[inline(always)]
    fn visit_assign_expr(&mut self, name: &str, value: &Expr, offset: &usize) -> ResolverResult<()> {
        self.visit_expr(value)?;
        self.resolve_local(name, *offset);
        Ok(())
    }

    #[inline(always)]
    fn visit_binary_expr(&mut self, left: &Expr, right: &Expr) -> ResolverResult<()> {
        self.visit_expr(left)?;
        self.visit_expr(right)
    }

    #[inline(always)]
    fn visit_call_expr(&mut self, callee: &Expr, args: &[Expr]) -> ResolverResult<()> {
        self.visit_expr(callee)?;

        for arg in args {
            self.visit_expr(arg)?;
        }

        Ok(())
    }

    #[inline(always)]
    fn visit_ternary_expr(&mut self, condition: &Expr, then_branch: &Expr, else_branch: &Expr) -> ResolverResult<()> {
        self.visit_expr(condition)?;
        self.visit_expr(then_branch)?;
        self.visit_expr(else_branch)
    }

    #[inline(always)]
    fn visit_variable_expr(&mut self, name: &str, line: &usize, offset: &usize) -> ResolverResult<()> {
        if let Some(scope) = self.scopes.last() {
            if let Some(VariableData { state: VariableState::Declared, .. }) = scope.get(name) {
                return Err(ResolverError::SelfRefInitializer { name: name.to_owned(), line: *line })
            }
        }

        self.resolve_local(name, *offset);

        Ok(())
    }

    #[inline(always)]
    fn visit_logical_expr(&mut self, left: &Expr, right: &Expr) -> ResolverResult<()> {
        self.visit_expr(left)?;
        self.visit_expr(right)
    }

    #[inline(always)]
    fn resolve_function(
        &mut self,
        params: &[(String, usize)],
        body: &[Stmt],
        line: usize,
        kind: FunctionKind
    ) -> ResolverResult<()> {
        let enclosing_function = std::mem::replace(&mut self.current_function, kind);
        self.begin_scope();
        for (param, offset) in params {
            self.declare(param, line)?;
            self.define(param);
            self.resolve_local(param, *offset)
        }
        self.resolve_stmts(body)?;
        self.end_scope();
        self.current_function = enclosing_function;
        Ok(())
    }
}