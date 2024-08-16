use std::fmt::Display;

use crate::{expr::{BinaryOp, BinaryOpKind, Expr, Literal, UnaryOp, UnaryOpKind}, report, scanner::{Scanner, Token, TokenKind}, stmt::Stmt};

#[derive(Debug)]
enum ErrorKind {
    UnexpectedToken,
    UnmatchedParentheses,
    MissingLhs,
    InvalidAssignmentTarget,
    BreakOutsideLoop
}

#[derive(Debug)]
pub struct ParserError {
    msg: String,
    line: usize,
    kind: ErrorKind
}

impl Display for ParserError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.kind {
            ErrorKind::UnexpectedToken => write!(
                f,
                "[line {}] Error: Unexpected {}",
                self.line,
                self.msg
            ),
            ErrorKind::UnmatchedParentheses => write!(
                f,
                "[line {}] Error: Unmatched parentheses",
                self.line
            ),
            ErrorKind::MissingLhs => write!(
                f,
                "[line {}] Error: LHS missing for '{}'",
                self.line,
                self.msg
            ),
            ErrorKind::InvalidAssignmentTarget => write!(
                f,
                "[line {}] Error at '=': Invalid assignment target.",
                self.line
            ),
            ErrorKind::BreakOutsideLoop => write!(
                f,
                "[line {}] Error: `break` appeared outside a enclosing loop.",
                self.line
            ),
        }
    }
}

type ParserResult<T> = Result<T, ParserError>;

pub struct Parser<'a> {
    scanner: Scanner<'a>,
    token: Token<'a>,
    loop_depth: u16
}

impl<'a> Parser<'a> {
    #[inline(always)]
    pub fn new(source: &'a str) -> Self {
        let mut scanner = Scanner::new(source);

        scanner.skip_shebang();

        Self {
            token: scanner.scan_token(),
            scanner,
            loop_depth: 0,
        }
    }

    #[inline(always)]
    pub fn is_at_end(&self) -> bool {
        self.token.kind == TokenKind::EOF
    }

    #[inline(always)]
    fn advance(&mut self) {
        self.token = self.scanner.scan_token();
    }

    #[inline]
    fn synchronize(&mut self) {
        if self.token.kind == TokenKind::Semicolon {
            self.advance();
            return
        }
        
        self.advance();

        while !self.is_at_end() {
            match self.token.kind {
                TokenKind::Class
                | TokenKind::Fun
                | TokenKind::Var
                | TokenKind::For
                | TokenKind::If
                | TokenKind::While
                | TokenKind::Print
                | TokenKind::Return => return,
                TokenKind::Semicolon => { self.advance(); return},
                _ => ()
            }

            self.advance()
        }
    }

    #[inline(always)]
    pub fn parse(&mut self) -> ParserResult<Stmt> {
        self.declaration()
    }

    fn declaration(&mut self) -> ParserResult<Stmt> {
        let result = match self.token.kind {
            TokenKind::Class => self.class_statement(),
            TokenKind::Fun => { self.advance(); self.fun_statement() },
            TokenKind::Var => self.var_declaration(),
            _ => self.statement()
        };
        if let Err(ref e) = result {
            match self.token.kind {
                TokenKind::Error(_) => report(&self.token),
                _ => eprintln!("{}", e),
            }
            self.synchronize()
        }

        result
    }

    fn var_declaration(&mut self) -> ParserResult<Stmt> {
        self.advance();

        let name = if let TokenKind::Identifier = self.token.kind {
            self.token.get_lexeme().to_owned()
        } else {
            return Err(self.unexpected_token(format!("'{}'. Expected variable name.", self.token.get_lexeme())));
        };

        let line = self.token.get_line();
        let offset = self.token.get_offset();

        self.advance();
        
        let initializer = if self.token.kind == TokenKind::Equal {
            self.advance();
            Some(self.expression()?)
        } else {
            None
        };

        if self.token.kind == TokenKind::Semicolon {
            self.advance();
            Ok(Stmt::Var {name, initializer, line, offset})
        } else {
            Err(self.unexpected_token(format!("'{}'. Expected ';'.", self.token.get_lexeme())))
        }
    }

    fn statement(&mut self) -> ParserResult<Stmt> {
        match self.token.kind {
            TokenKind::Print => self.print_statement(),
            TokenKind::While => self.while_statement(),
            TokenKind::For => self.for_statement(),
            TokenKind::Break => self.break_statement(),
            TokenKind::Return => self.return_statement(),
            TokenKind::LeftBrace => {
                let mut block = self.block()?;
                if block.len() == 1 { // Optimization
                    Ok(block.swap_remove(0))
                } else {
                    Ok(Stmt::Block{statements: block})
                }
            },
            TokenKind::If => self.if_statement(),
            _ => self.expr_statement()
        }
    }

    fn print_statement(&mut self) -> ParserResult<Stmt> {
        self.advance();
        let statement = Stmt::Print(self.expression()?);
        if self.token.kind != TokenKind::Semicolon {
            return Err(self.unexpected_token(format!("'{}'. Expected ';'.", self.token.get_lexeme())))
        }
        self.advance();

        Ok(statement)
    }

    fn while_statement(&mut self) -> ParserResult<Stmt> {
        self.advance();
        if self.token.kind != TokenKind::LeftParen {
            return Err(self.unexpected_token(format!("'{}'. Expected '('.", self.token.get_lexeme())))
        }
        self.advance();
        
        let condition = self.expression()?;

        if self.token.kind != TokenKind::RightParen {
            return Err(self.unexpected_token(format!("'{}'. Expected ')'.", self.token.get_lexeme())))
        }
        self.advance();

        self.loop_depth += 1;
        let body = self.statement()?;
        self.loop_depth -= 1;

        Ok(Stmt::While {
            condition,
            body: Box::new(body)
        })
    }

    fn for_statement(&mut self) -> ParserResult<Stmt> {
        self.advance();
        if self.token.kind != TokenKind::LeftParen {
            return Err(self.unexpected_token(format!("'{}'. Expected '('.", self.token.get_lexeme())))
        }
        self.advance();

        let initializer = match self.token.kind {
            TokenKind::Semicolon => {
                self.advance();
                None
            },
            TokenKind::Var => Some(self.var_declaration()?),
            _ => Some(self.expr_statement()?),
        };

        let condition = match self.token.kind {
            TokenKind::Semicolon => Expr::Literal(Literal::Bool(true)),
            _ => self.expression()?
        };

        if self.token.kind != TokenKind::Semicolon {
            return Err(self.unexpected_token(format!("'{}'. Expected ';'.", self.token.get_lexeme())))
        }
        self.advance();

        let increment = match self.token.kind {
            TokenKind::RightParen => None,
            _ => Some(self.expression()?)
        };
        
        if self.token.kind != TokenKind::RightParen {
            return Err(self.unexpected_token(format!("'{}'. Expected ')'.", self.token.get_lexeme())))
        }
        self.advance();

        self.loop_depth += 1;
        let mut body = self.statement()?;
        self.loop_depth -= 1;

        if let Some(increment) = increment {
            if let Stmt::Block { ref mut statements } = body {
                statements.push(Stmt::Expr(increment));
            } else {
                body = Stmt::Block { statements: vec![body, Stmt::Expr(increment)] }
            }
        };

        body = Stmt::While {
            condition,
            body: Box::new(body)
        };

        if let Some(initializer) = initializer {
            body = Stmt::Block { statements: vec![initializer, body] }
        };

        Ok(body)

    }

    fn break_statement(&mut self) -> ParserResult<Stmt> {
        let line = self.token.get_line();
        
        self.advance();

        if self.token.kind != TokenKind::Semicolon {
            return Err(self.unexpected_token(format!("'{}'. Expected ';'.", self.token.get_lexeme())))
        }

        if self.loop_depth > 0 {
            self.advance();
            Ok(Stmt::Break { line: self.token.get_line() })
        } else {
            Err(ParserError {
                msg: String::new(),
                line,
                kind: ErrorKind::BreakOutsideLoop,
            })
        }

    }

    fn class_statement(&mut self) -> ParserResult<Stmt> {
        let line = self.token.get_line();
        self.advance();

        if self.token.kind != TokenKind::Identifier {
            return Err(self.unexpected_token(format!("'{}'. Expected class name.", self.token.get_lexeme())))
        }

        let name = self.token.get_lexeme().to_owned();
        let offset = self.token.get_offset();

        self.advance();
        if self.token.kind != TokenKind::LeftBrace {
            return Err(self.unexpected_token(format!("'{}'. Expected '{{'.", self.token.get_lexeme())))
        }
        self.advance();

        let methods = if self.token.kind == TokenKind::RightBrace {
            Vec::new()
        } else {
            let mut methods = Vec::with_capacity(4);
            methods.push(self.fun_statement()?);
            while !self.is_at_end() && self.token.kind != TokenKind::RightBrace {
                methods.push(self.fun_statement()?);
            }

            methods
        };

        if self.token.kind != TokenKind::RightBrace {
            return Err(self.unexpected_token(format!("'{}'. Expected '}}'.", self.token.get_lexeme())))
        }
        self.advance();


        Ok(Stmt::Class {
            name,
            methods,
            line,
            offset
        })
    }

    fn fun_statement(&mut self) -> ParserResult<Stmt> {
        if self.token.kind != TokenKind::Identifier {
            return Err(self.unexpected_token(format!("'{}'. Expected function name.", self.token.get_lexeme())))
        }
        let name = self.token.get_lexeme().to_owned();
        let line = self.token.get_line();
        let offset = self.token.get_offset();
        self.advance();
        if self.token.kind != TokenKind::LeftParen {
            return Err(self.unexpected_token(format!("'{}'. Expected '('.", self.token.get_lexeme())))
        }
        self.advance();

        let params = if self.token.kind != TokenKind::RightParen {
            let mut params = Vec::with_capacity(4);
            params.push((self.token.get_lexeme().to_owned(), self.token.get_offset()));
            self.advance();
            while !self.is_at_end() && self.token.kind == TokenKind::Comma {
                if params.len() > 255 {
                    eprintln!("[line {}] Error: Can't have more than 255 arguments.", self.token.get_line());
                }
                self.advance();
                params.push((self.token.get_lexeme().to_owned(), self.token.get_offset()));
                self.advance();
            } 
            params
        } else {
            Vec::new()
        };

        if self.token.kind != TokenKind::RightParen {
            return Err(self.unexpected_token(format!("'{}'. Expected ')'.", self.token.get_lexeme())))
        }
        self.advance();
        if self.token.kind != TokenKind::LeftBrace {
            return Err(self.unexpected_token(format!("'{}'. Expected '{{'.", self.token.get_lexeme())))
        }
        let body = self.block()?;

        Ok(Stmt::Function {
            name,
            params,
            body,
            line,
            offset
        })
    }

    fn return_statement(&mut self) -> ParserResult<Stmt> {
        let line = self.token.get_line();
        self.advance();

        let value = if self.token.kind == TokenKind::Semicolon {
            Expr::Literal(Literal::Nil)
        } else {
            self.expression()?
        };

        if self.token.kind != TokenKind::Semicolon {
            return Err(self.unexpected_token(format!("'{}'. Expected ';'.", self.token.get_lexeme())))
        }
        self.advance();

        Ok(Stmt::Return {
            line,
            value
        })
    }

    fn block(&mut self) -> ParserResult<Vec<Stmt>> {
        self.advance();
        let statements = if self.token.kind == TokenKind::RightBrace {
            Vec::new()
        } else {
            let mut statements =  Vec::with_capacity(8);
            while !self.is_at_end() && self.token.kind != TokenKind::RightBrace {
                statements.push(self.declaration()?);
            }
            statements
        };

        if self.token.kind != TokenKind::RightBrace {
            Err(self.unexpected_token(format!("'{}'. Expected '}}' after block.", self.token.get_lexeme())))
        } else {
            self.advance();
            Ok(statements)
        }
    }

    fn if_statement(&mut self) -> ParserResult<Stmt> {
        self.advance();
        if self.token.kind != TokenKind::LeftParen {
            return Err(self.unexpected_token(format!("'{}'. Expected '('.", self.token.get_lexeme())))
        }
        self.advance();
        let condition = self.expression()?;
        if self.token.kind != TokenKind::RightParen {
            return Err(self.unexpected_token(format!("'{}'. Expected ')'.", self.token.get_lexeme())))
        }
        self.advance();

        let then_branch = self.statement()?;
        let else_branch = if self.token.kind == TokenKind::Else {
            self.advance();
            Some(Box::new(self.statement()?))
        } else {
            None
        };
        
        Ok(Stmt::If {
            condition,
            then_branch: Box::new(then_branch),
            else_branch
        })
    }

    fn expr_statement(&mut self) -> ParserResult<Stmt> {
        let statement = Stmt::Expr(self.expression()?);
        if self.token.kind != TokenKind::Semicolon {
            return Err(self.unexpected_token(format!("'{}'. Expected ';'.", self.token.get_lexeme())))
        }
        self.advance();
        
        Ok(statement)
    }

    #[inline(always)]
    pub fn expression(&mut self) -> ParserResult<Expr> {
        if matches!(
            self.token.kind,
            TokenKind::Plus
            | TokenKind::Star
            | TokenKind::Slash
            | TokenKind::EqualEqual
            | TokenKind::BangEqual
            | TokenKind::Less
            | TokenKind::LessEqual
            | TokenKind::Greater
            | TokenKind::GreaterEqual
            | TokenKind::And
            | TokenKind::Or
        ) {
            let e = ParserError {
                msg: self.token.get_lexeme().to_owned(),
                line: self.token.get_line(),
                kind: ErrorKind::MissingLhs
            };
            return Err(e)
        }

        self.assignment()
    }

    fn assignment(&mut self) -> ParserResult<Expr> {
        if self.token.kind == TokenKind::Fun {
            return Ok(self.lambda()?)
        }
        let mut expr = self.ternary()?;

        if self.token.kind == TokenKind::Equal {
            self.advance();
            if let Expr::Variable { name, line, offset } = expr {
                expr = Expr::Assign { name, value: Box::new(self.assignment()?), line, offset }
            } else if let Expr::Get { name, object, line } = expr {
                expr = Expr::Set { name, object, value: Box::new(self.assignment()?), line }
            } else {
                return Err(
                    ParserError {
                        msg: String::new(),
                        line: self.token.get_line(),
                        kind: ErrorKind::InvalidAssignmentTarget,
                    }
                )
            }
        }

        Ok(expr)
    }

    fn ternary(&mut self) -> ParserResult<Expr> {
        let mut expr = self.or()?;
        
        while self.token.kind == TokenKind::QuestionMark {
            self.advance();
            let then_branch = self.equality()?;
            
            if self.token.kind != TokenKind::Colon {
                return Err(self.unexpected_token(format!("'{}'. Expected ':'.", self.token.get_lexeme())))
            }
            self.advance();
            let else_branch = self.equality()?;
            expr = Expr::Ternary {
                exprs: Box::new((expr, then_branch, else_branch)),
            }
        }
        
        Ok(expr)
    }
    
    fn or(&mut self) -> ParserResult<Expr> {
        let mut expr = self.and()?;

        while self.token.kind == TokenKind::Or {
            let operator = BinaryOp {
                line: self.token.get_line(),
                kind: BinaryOpKind::Or
            };
            self.advance();
            let right = self.and()?;
            expr = Expr::Logical {
                values: Box::new((expr, right)),
                operator
            };
        }

        Ok(expr)
    }

    fn and(&mut self) -> ParserResult<Expr> {
        let mut expr = self.equality()?;

        while self.token.kind == TokenKind::And {
            let operator = BinaryOp {
                line: self.token.get_line(),
                kind: BinaryOpKind::And
            };
            self.advance();

            let right = self.equality()?;
            expr = Expr::Logical {
                values: Box::new((expr, right)),
                operator
            };
        }

        Ok(expr)
    }

    // TODO Create a helper method for parsing a left-associative series of binary operators
    fn equality(&mut self) -> ParserResult<Expr> {
        let mut expr = self.comparison()?;

        while matches!(self.token.kind, TokenKind::EqualEqual | TokenKind::BangEqual) {
            let operator = self.binary_operator()?;
            let right = self.comparison()?;
            expr = Expr::Binary {
                values: Box::new((expr, right)),
                operator
            };
        }

        Ok(expr)
    }

    fn comparison(&mut self) -> ParserResult<Expr> {
        let mut expr = self.term()?;

        while matches!(self.token.kind, TokenKind::Greater | TokenKind::GreaterEqual | TokenKind::Less | TokenKind::LessEqual) {
            let operator = self.binary_operator()?;
            let right = self.term()?;
            expr = Expr::Binary {
                values: Box::new((expr, right)),
                operator
            };
        }

        Ok(expr)
    }

    fn term(&mut self) -> ParserResult<Expr> {
        let mut expr = self.factor()?;

        while matches!(self.token.kind, TokenKind::Minus | TokenKind::Plus) {
            let operator = self.binary_operator()?;
            let right = self.factor()?;
            expr = Expr::Binary {
                values: Box::new((expr, right)),
                operator
            };
        }

        Ok(expr)
    }

    fn factor(&mut self) -> ParserResult<Expr> {
        let mut expr = self.unary()?;

        while matches!(self.token.kind, TokenKind::Slash | TokenKind::Star) {
            let operator = self.binary_operator()?;
            let right = self.unary()?;
            expr = Expr::Binary {
                values: Box::new((expr, right)),
                operator
            };
        }

        Ok(expr)
    }

    fn unary(&mut self) -> ParserResult<Expr> {
        if matches!(self.token.kind, TokenKind::Minus | TokenKind::Bang) {
            let operator = self.unary_operator()?;
            let right = self.unary()?;
            return Ok(Expr::Unary {
                operator,
                right: Box::new(right)
            })
        }

        self.call()
    }

    fn call(&mut self) -> ParserResult<Expr> {
        let mut expr = self.primary()?;

        loop {
            match self.token.kind {
                TokenKind::LeftParen => {
                    self.advance();
                    expr = self.finish_call(expr)?;
                },
                TokenKind::Dot => {
                    self.advance();
                    if self.token.kind != TokenKind::Identifier {
                        return Err(self.unexpected_token(format!("'{}'. Expected property name.", self.token.get_lexeme())))
                    }
                    expr = Expr::Get { 
                        name: self.token.get_lexeme().to_owned(),
                        object: Box::new(expr),
                        line: self.token.get_line()
                    };
                    self.advance();
                },
                _ => break
            } 
        }

        Ok(expr)
    }

    #[inline(always)]
    fn finish_call(&mut self, expr: Expr) -> ParserResult<Expr> {
        let exprs = if self.token.kind != TokenKind::RightParen {
            let mut exprs = Vec::with_capacity(4);
            exprs.push(expr);
            exprs.push(self.expression()?);
            while self.token.kind == TokenKind::Comma {
                self.advance();
                if exprs.len() > 255 {
                    eprintln!("[line {}] Error: Can't have more than 255 arguments.", self.token.get_line());
                }
                exprs.push(self.expression()?)
            }
            exprs
        } else {
            vec![expr]
        };

        if self.token.kind != TokenKind::RightParen {
            return Err(self.unexpected_token(format!("'{}'. Expected ')'.", self.token.get_lexeme())))
        }

        let line = self.token.get_line();
        self.advance();

        Ok(Expr::Call {
            line,
            exprs
        })
    }

    #[inline(always)]
    fn lambda(&mut self) -> ParserResult<Expr> {
        self.advance();
        if self.token.kind != TokenKind::LeftParen {
            return Err(self.unexpected_token(format!("'{}'. Expected '('.", self.token.get_lexeme())));
        }
        self.advance();
        
        let params = if self.token.kind == TokenKind::RightParen {
            Vec::new()
        } else {
            let mut params = Vec::with_capacity(4);
            if self.token.kind != TokenKind::Identifier {
                return Err(self.unexpected_token(format!("'{}'. Expected identifier.", self.token.get_lexeme())));
            }
            params.push((self.token.get_lexeme().to_owned(), self.token.get_offset()));
            self.advance();
            while self.token.kind == TokenKind::Comma {
                self.advance();
                if self.token.kind != TokenKind::Identifier {
                    return Err(self.unexpected_token(format!("'{}'. Expected identifier.", self.token.get_lexeme())));
                }
                params.push((self.token.get_lexeme().to_owned(), self.token.get_offset()));
                self.advance();
            }
            params
        };

        if self.token.kind != TokenKind::RightParen {
            return Err(self.unexpected_token(format!("'{}'. Expected ')'.", self.token.get_lexeme())));
        }
        self.advance();
        if self.token.kind != TokenKind::LeftBrace {
            return Err(self.unexpected_token(format!("'{}'. Expected '{{'.", self.token.get_lexeme())));
        }

        Ok(Expr::Lambda {
            params,
            body: self.block()?
        })
    }

    #[inline]
    fn primary(&mut self) -> ParserResult<Expr> {
        let expr = match self.token.kind {
            TokenKind::False => Ok(Expr::Literal(Literal::Bool(false))), 
            TokenKind::True => Ok(Expr::Literal(Literal::Bool(true))),
            TokenKind::Nil => Ok(Expr::Literal(Literal::Nil)),
            TokenKind::Number => Ok(Expr::Literal(Literal::Number(self.token.get_lexeme().parse().unwrap()))),
            TokenKind::String => {
                let lexeme = self.token.get_lexeme();
                Ok(Expr::Literal(Literal::String(lexeme[1..lexeme.len() - 1].to_owned())))
            },
            TokenKind::LeftParen => {
                self.advance();
                let expr = self.expression()?;
                if self.token.kind != TokenKind::RightParen {
                    return Err(ParserError{
                        msg: String::new(),
                        line: self.token.get_line(),
                        kind: ErrorKind::UnmatchedParentheses
                    });
                }
                Ok(expr)
            }
            TokenKind::Identifier => Ok(Expr::Variable {name: self.token.get_lexeme().to_owned(), line: self.token.get_line(), offset: self.token.get_offset()}),
            _ => return Err(self.unexpected_token(format!("'{}'", self.token.get_lexeme())))
        };
        self.advance();
        expr
    }

    #[inline]
    fn binary_operator(&mut self) -> ParserResult<BinaryOp> {
        let kind = match self.token.kind {
            TokenKind::Plus => Ok(BinaryOpKind::Add),
            TokenKind::Minus => Ok(BinaryOpKind::Sub),
            TokenKind::Star => Ok(BinaryOpKind::Mul),
            TokenKind::Slash => Ok(BinaryOpKind::Div),
            TokenKind::EqualEqual => Ok(BinaryOpKind::Eq),
            TokenKind::BangEqual => Ok(BinaryOpKind::Ne),
            TokenKind::Less => Ok(BinaryOpKind::Lt),
            TokenKind::LessEqual => Ok(BinaryOpKind::Le),
            TokenKind::Greater => Ok(BinaryOpKind::Gt),
            TokenKind::GreaterEqual => Ok(BinaryOpKind::Ge),
            TokenKind::And => Ok(BinaryOpKind::And),
            TokenKind::Or => Ok(BinaryOpKind::Or),
            _ => return Err(self.unexpected_token(format!("'{}'. Expected binary operator.", self.token.get_lexeme())))
        }?;
        self.advance();
        Ok(BinaryOp {
            line: self.token.get_line(),
            kind,
        })
    }

    #[inline(always)]
    fn unary_operator(&mut self) -> ParserResult<UnaryOp> {
        let kind = match self.token.kind {
            TokenKind::Bang => Ok(UnaryOpKind::Not),
            TokenKind::Minus => Ok(UnaryOpKind::Neg),
            _ => return Err(self.unexpected_token(format!("'{}'. Expected unary operator.", self.token.get_lexeme())))
        }?;
        self.advance();
        Ok(UnaryOp {
            line: self.token.get_line(),
            kind,
        })
    }

    #[inline(always)]
    fn unexpected_token(&self, msg: String) -> ParserError {
        ParserError {
            msg,
            line: self.token.get_line(),
            kind: ErrorKind::UnexpectedToken,
        }
    }
}