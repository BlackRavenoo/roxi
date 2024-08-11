use std::{collections::HashMap, fmt::Display};

use wyhash2::WyHash;

use crate::{expr::{Expr, ExprVisitor}, interpreter::Interpreter, stmt::{Stmt, StmtVisitor}};

#[derive(Debug)]
pub enum ResolverError {
    SelfRefInitializer {
        name: String,
        line: usize
    }
    
}

impl Display for ResolverError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ResolverError::SelfRefInitializer { name, line } => write!(
                f,
                "[line {}] Can't read local variable `{}` in its own initializer.",
                line,
                name
            ),
        }
    }
}

pub type ResolverResult<T> = Result<T, ResolverError>;

pub struct Resolver<'a> {
    interpreter: &'a mut Interpreter,
    scopes: Vec<HashMap<String, bool, WyHash>>
}

impl StmtVisitor<ResolverResult<()>> for Resolver<'_> {
    #[inline(always)]
    fn visit_stmt(&mut self, stmt: &Stmt) -> ResolverResult<()> {
        match stmt {
            Stmt::Expr(expr) => self.visit_expr_stmt(expr),
            Stmt::Print(expr) => self.visit_expr_stmt(expr),
            Stmt::Return { value, .. } => self.visit_expr_stmt(value),
            Stmt::Var { name, initializer } => self.visit_var_stmt(name, initializer),
            Stmt::Block { statements } => self.visit_block_stmt(statements),
            Stmt::Function { name, params, body } => self.visit_function_stmt(name, params, body),
            Stmt::If { condition, then_branch, else_branch } => self.visit_if_stmt(condition, then_branch, else_branch),
            Stmt::While { condition, body } => self.visit_while_stmt(condition, body),
            Stmt::Break { .. } => Ok(()),
        }
    }
}

impl ExprVisitor<ResolverResult<()>> for Resolver<'_> {
    #[inline(always)]
    fn visit_expr(&mut self, expr: &Expr) -> ResolverResult<()> {
        match expr {
            Expr::Assign { name, value, .. } => self.visit_assign_expr(name, value),
            Expr::Binary { values, .. } => self.visit_binary_expr(&values.0, &values.1),
            Expr::Call { exprs, .. } => self.visit_call_expr(&exprs[0], &exprs[1..]),
            Expr::Literal(_) => Ok(()),
            Expr::Unary { right, .. } => self.visit_expr(right),
            Expr::Ternary { exprs } => self.visit_ternary_expr(&exprs.0, &exprs.1, &exprs.2),
            Expr::Variable { name, line } => self.visit_variable_expr(name, line),
            Expr::Logical { values, .. } => self.visit_logical_expr(&values.0, &values.1),
            Expr::Lambda { params, body } => self.resolve_function(params, body),
        }
    }
}

impl Resolver<'_> {
    #[inline(always)]
    pub fn new(interpreter: &mut Interpreter) -> Resolver {
        Resolver {
            interpreter,
            scopes: Vec::new(),
        }
    }

    fn resolve_stmts(&mut self, stmts: &[Stmt]) -> ResolverResult<()> {
        for stmt in stmts.iter() {
            self.visit_stmt(stmt)?
        };

        Ok(())
    }

    fn resolve_local(&mut self, name: &str) {
        self.scopes.iter().rev().enumerate().for_each(|(i, scope)| {
            if scope.contains_key(name) {
                self.interpreter.resolve(name, self.scopes.len() - 1 - i);
                return;
            }
        })
    }
    
    fn begin_scope(&mut self) {
        self.scopes.push(HashMap::with_hasher(WyHash::with_seed(0)))
    }
    
    fn end_scope(&mut self) {
        self.scopes.pop();
    }

    fn declare(&mut self, name: &str) {
        if let Some(scope) = self.scopes.last_mut() {
            scope.insert(name.to_string(), false);
        }
    }

    fn define(&mut self, name: &str) {
        if let Some(scope) = self.scopes.last_mut() {
            scope.insert(name.to_string(), true);
        }
    }

    #[inline(always)]
    fn visit_expr_stmt(&mut self, expr: &Expr) -> ResolverResult<()> {
        self.visit_expr(expr)
    }
    
    #[inline(always)]
    fn visit_var_stmt(&mut self, name: &str, initializer: &Option<Expr>) -> ResolverResult<()>  {
        self.declare(name);
        if let Some(initializer) = initializer {
            self.visit_expr(initializer);
        }
        self.define(name);
        Ok(())
    }

    #[inline(always)]
    fn visit_block_stmt(&mut self, statements: &[Stmt]) -> ResolverResult<()> {
        self.begin_scope();
        self.resolve_stmts(statements);
        self.end_scope();
        Ok(())
    }

    #[inline(always)]
    fn visit_function_stmt(&mut self, name: &str, params: &[String], body: &[Stmt]) -> ResolverResult<()> {
        self.declare(name);
        self.define(name);
        self.resolve_function(params, body);

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
    fn visit_assign_expr(&mut self, name: &str, value: &Expr) -> ResolverResult<()> {
        self.visit_expr(value);
        self.resolve_local(name);
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
    fn visit_variable_expr(&mut self, name: &str, line: &usize) -> ResolverResult<()> {
        if let Some(scope) = self.scopes.last() {
            if scope.get(name) == Some(&false) {
                return Err(ResolverError::SelfRefInitializer { name: name.to_owned(), line: *line })
            }
        }

        self.resolve_local(name);

        Ok(())
    }

    #[inline(always)]
    fn visit_logical_expr(&mut self, left: &Expr, right: &Expr) -> ResolverResult<()> {
        self.visit_expr(left)?;
        self.visit_expr(right)
    }

    #[inline(always)]
    fn resolve_function(&mut self, params: &[String], body: &[Stmt]) -> ResolverResult<()> {
        self.begin_scope();
        for param in params {
            self.declare(param);
            self.define(param);
        }
        self.resolve_stmts(body);
        self.end_scope();
        Ok(())
    }
}