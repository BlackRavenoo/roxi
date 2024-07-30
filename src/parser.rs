use std::fmt::Display;

use crate::{expr::{BinaryOp, BinaryOpKind, Expr, Literal, UnaryOp, UnaryOpKind}, report, scanner::{Scanner, Token, TokenKind}};

#[derive(Debug)]
enum ErrorKind {
    UnexpectedToken,
    UnmatchedParentheses,
    MissingLhs
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
    pub fn expression(&mut self) -> ParserResult<Expr<'a>> {
        while matches!(
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
            eprintln!("{}", e);
            self.synchronize();
            if self.is_at_end() {
                return Err(e)
            }
        }
        let result = self.ternary();
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

    fn ternary(&mut self) -> ParserResult<Expr<'a>> {
        let mut expr = self.equality()?;

        while self.token.kind == TokenKind::QuestionMark {
            self.advance();
            let then_branch = self.equality()?;
            
            if self.token.kind != TokenKind::Colon {
                return Err(ParserError{
                    msg: format!("{}. Expected ':' character.", self.token.get_lexeme()),
                    line: self.token.get_line(),
                    kind: ErrorKind::UnexpectedToken
                });
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

        while [TokenKind::EqualEqual, TokenKind::BangEqual].contains(&self.token.kind) {
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

        while [TokenKind::Greater, TokenKind::GreaterEqual, TokenKind::Less, TokenKind::LessEqual].contains(&self.token.kind) {
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

        while [TokenKind::Minus, TokenKind::Plus].contains(&self.token.kind) {
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

        while [TokenKind::Slash, TokenKind::Star].contains(&self.token.kind) {
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
        if [TokenKind::Minus, TokenKind::Bang].contains(&self.token.kind) {
            let operator = self.unary_operator()?;
            let right = self.unary()?;
            return Ok(Expr::Unary {
                operator,
                right: Box::new(right)
            })
        }

        self.primary()
    }

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
            _ => return Err(ParserError {
                msg: self.token.get_lexeme().to_owned(),
                line: self.token.get_line(),
                kind: ErrorKind::UnexpectedToken,
            })
        };
        self.advance();
        expr
    }

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
            _ => return Err(ParserError {
                msg: format!("{}. Expected binary operator.", self.token.get_lexeme()),
                line: self.token.get_line(),
                kind: ErrorKind::UnexpectedToken,
            })
        }?;
        self.advance();
        Ok(BinaryOp {
            line: self.token.get_line(),
            kind,
        })
    }

    fn unary_operator(&mut self) -> ParserResult<UnaryOp> {
        let kind = match self.token.kind {
            TokenKind::Bang => Ok(UnaryOpKind::Not),
            TokenKind::Minus => Ok(UnaryOpKind::Neg),
            _ => return Err(ParserError {
                msg: format!("{}. Expected unary operator.", self.token.get_lexeme()),
                line: self.token.get_line(),
                kind: ErrorKind::UnexpectedToken,
            })
        }?;
        self.advance();
        Ok(UnaryOp {
            line: self.token.get_line(),
            kind,
        })
    }
}