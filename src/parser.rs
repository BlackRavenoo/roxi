use std::fmt::Display;

use crate::{expr::{BinaryOp, BinaryOpKind, Expr, Literal, UnaryOp, UnaryOpKind}, report, scanner::{Scanner, Token, TokenKind}, stmt::Stmt};

#[derive(Debug)]
enum ErrorKind {
    UnexpectedToken,
    UnmatchedParentheses,
    MissingLhs,
    InvalidAssignmentTarget,
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
                "[line {}] Error: Unexpected '{}'",
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
        }
    }
}

type ParserResult<T> = Result<T, ParserError>;

pub struct Parser<'a> {
    scanner: Scanner<'a>,
    token: Token<'a>,
}

impl<'a> Parser<'a> {
    #[inline(always)]
    pub fn new(source: &'a str) -> Self {
        let mut scanner = Scanner::new(source);

        scanner.skip_shebang();

        Self {
            token: scanner.scan_token(),
            scanner,
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

    #[inline]
    pub fn parse(&mut self) -> ParserResult<Stmt<'a>> {
        self.declaration()
    }

    fn declaration(&mut self) -> ParserResult<Stmt<'a>> {
        let result = match self.token.kind {
            TokenKind::Var => self.var_declaration(),
            _ => self.statement()
        };
        if let Err(ref e) = result {
            match self.token.kind {
                TokenKind::EOF => eprintln!("Error: Unexpected EOF"),
                TokenKind::Error(_) => report(&self.token),
                _ => eprintln!("{}", e),
            }
            self.synchronize()
        }

        result
    }

    fn var_declaration(&mut self) -> ParserResult<Stmt<'a>> {
        self.advance();

        let name = if let TokenKind::Identifier = self.token.kind {
            self.token.get_lexeme()
        } else {
            return Err(self.unexpected_token(format!("{}. Expected variable name.", self.token.get_lexeme())));
        };

        self.advance();
        
        let initializer = if self.token.kind == TokenKind::Equal {
            self.advance();
            Some(self.expression()?)
        } else {
            None
        };

        if self.token.kind == TokenKind::Semicolon {
            self.advance();
            Ok(Stmt::Var {name, initializer})
        } else {
            Err(self.unexpected_token(format!("{}. Expected ';'.", self.token.get_lexeme())))
        }
    }

    fn statement(&mut self) -> ParserResult<Stmt<'a>> {
        match self.token.kind {
            TokenKind::Print => self.print_statement(),
            TokenKind::LeftBrace => Ok(Stmt::Block{statements: self.block()?}),
            _ => self.expr_statement()
        }
    }

    fn print_statement(&mut self) -> ParserResult<Stmt<'a>> {
        self.advance();
        let statement = Stmt::Print(self.expression()?);
        if self.token.kind != TokenKind::Semicolon {
            return Err(self.unexpected_token(format!("{}. Expected ';'.", self.token.get_lexeme())))
        }
        self.advance();

        Ok(statement)
    }

    fn block(&mut self) -> ParserResult<Vec<Stmt<'a>>> {
        self.advance();
        let mut statements = Vec::with_capacity(32);
        while !self.is_at_end() && self.token.kind != TokenKind::RightBrace {
            statements.push(self.declaration()?);
        }

        if self.token.kind != TokenKind::RightBrace {
            todo!() //Err
        } else {
            self.advance();
            Ok(statements)
        }
    }

    fn expr_statement(&mut self) -> ParserResult<Stmt<'a>> {
        let statement = Stmt::Expr(self.expression()?);
        if self.token.kind != TokenKind::Semicolon {
            return Err(self.unexpected_token(format!("{}. Expected ';'.", self.token.get_lexeme())))
        }
        self.advance();
        
        Ok(statement)
    }

    #[inline(always)]
    pub fn expression(&mut self) -> ParserResult<Expr<'a>> {
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

    fn assignment(&mut self) -> ParserResult<Expr<'a>> {
        let mut expr = self.ternary()?;

        if self.token.kind == TokenKind::Equal {
            self.advance();
            if let Expr::Variable { name, line } = expr {
                expr = Expr::Assign { name, value: Box::new(self.assignment()?), line }
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

    fn ternary(&mut self) -> ParserResult<Expr<'a>> {
        let mut expr = self.equality()?;

        while self.token.kind == TokenKind::QuestionMark {
            self.advance();
            let then_branch = self.equality()?;
            
            if self.token.kind != TokenKind::Colon {
                return Err(self.unexpected_token(format!("{}. Expected ':'.", self.token.get_lexeme())))
            }
            self.advance();
            let else_branch = self.equality()?;
            expr = Expr::Ternary {
                condition: Box::new(expr),
                then_branch: Box::new(then_branch),
                else_branch: Box::new(else_branch)
            }
        }

        Ok(expr)
    }
    
    // TODO Create a helper method for parsing a left-associative series of binary operators
    fn equality(&mut self) -> ParserResult<Expr<'a>> {
        let mut expr = self.comparison()?;

        while matches!(self.token.kind, TokenKind::EqualEqual | TokenKind::BangEqual) {
            let operator = self.binary_operator()?;
            let right = self.comparison()?;
            expr = Expr::Binary {
                left: Box::new(expr),
                operator,
                right: Box::new(right)
            };
        }

        Ok(expr)
    }

    fn comparison(&mut self) -> ParserResult<Expr<'a>> {
        let mut expr = self.term()?;

        while matches!(self.token.kind, TokenKind::Greater | TokenKind::GreaterEqual | TokenKind::Less | TokenKind::LessEqual) {
            let operator = self.binary_operator()?;
            let right = self.term()?;
            expr = Expr::Binary {
                left: Box::new(expr),
                operator,
                right: Box::new(right)
            };
        }

        Ok(expr)
    }

    fn term(&mut self) -> ParserResult<Expr<'a>> {
        let mut expr = self.factor()?;

        while matches!(self.token.kind, TokenKind::Minus | TokenKind::Plus) {
            let operator = self.binary_operator()?;
            let right = self.factor()?;
            expr = Expr::Binary {
                left: Box::new(expr),
                operator,
                right: Box::new(right)
            };
        }

        Ok(expr)
    }

    fn factor(&mut self) -> ParserResult<Expr<'a>> {
        let mut expr = self.unary()?;

        while matches!(self.token.kind, TokenKind::Slash | TokenKind::Star) {
            let operator = self.binary_operator()?;
            let right = self.unary()?;
            expr = Expr::Binary {
                left: Box::new(expr),
                operator,
                right: Box::new(right)
            };
        }

        Ok(expr)
    }

    fn unary(&mut self) -> ParserResult<Expr<'a>> {
        if matches!(self.token.kind, TokenKind::Minus | TokenKind::Bang) {
            let operator = self.unary_operator()?;
            let right = self.unary()?;
            return Ok(Expr::Unary {
                operator,
                right: Box::new(right)
            })
        }

        self.primary()
    }

    #[inline]
    fn primary(&mut self) -> ParserResult<Expr<'a>> {
        let expr = match self.token.kind {
            TokenKind::False => Ok(Expr::Literal(Literal::Bool(false))), 
            TokenKind::True => Ok(Expr::Literal(Literal::Bool(true))),
            TokenKind::Nil => Ok(Expr::Literal(Literal::Nil)),
            TokenKind::Number => Ok(Expr::Literal(Literal::Number(self.token.get_lexeme().parse().unwrap()))),
            TokenKind::String => {
                let lexeme = self.token.get_lexeme();
                Ok(Expr::Literal(Literal::String(&lexeme[1..lexeme.len() - 1])))
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
                Ok(Expr::Grouping {expression: Box::new(expr)})
            }
            TokenKind::Identifier => Ok(Expr::Variable {name: self.token.get_lexeme(), line: self.token.get_line()}),
            _ => return Err(self.unexpected_token(self.token.get_lexeme().to_owned()))
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
            _ => return Err(self.unexpected_token(format!("{}. Expected binary operator.", self.token.get_lexeme())))
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
            _ => return Err(self.unexpected_token(format!("{}. Expected unary operator.", self.token.get_lexeme())))
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