use std::fmt::{Debug, Display};

static KEYWORDS: phf::Map<&'static str, TokenKind> = phf::phf_map!(
    "and" => TokenKind::And,
    "class" => TokenKind::Class,
    "else" => TokenKind::Else,
    "false" => TokenKind::False,
    "fun" => TokenKind::Fun,
    "for" => TokenKind::For,
    "if" => TokenKind::If,
    "nil" => TokenKind::Nil,
    "or" => TokenKind::Or,
    "print" => TokenKind::Print,
    "return" => TokenKind::Return,
    "super" => TokenKind::Super,
    "this" => TokenKind::This,
    "true" => TokenKind::True,
    "var" => TokenKind::Var,
    "while" => TokenKind::While,
);

pub struct Scanner<'a> {
    source: &'a str,

    start: usize,
    current: usize,

    line: usize,
}

impl<'a> Scanner<'a> {
    pub fn new(source: &'a str) -> Self {
        Self {
            source,
            start: 0,
            current: 0,
            line: 1
        }
    }

    pub fn scan_token(&mut self) -> Token<'a> {
        self.skip_whitespace();
        self.start = self.current;

        if self.is_at_end() {
            return self.make_token(TokenKind::EOF);
        }

        match self.advance() {
            b'(' => self.make_token(TokenKind::LeftParen),
            b')' => self.make_token(TokenKind::RightParen),
            b'{' => self.make_token(TokenKind::LeftBrace),
            b'}' => self.make_token(TokenKind::RightBrace),
            b'*' => self.make_token(TokenKind::Star),
            b'.' => self.make_token(TokenKind::Dot),
            b',' => self.make_token(TokenKind::Comma),
            b'+' => self.make_token(TokenKind::Plus),
            b'-' => self.make_token(TokenKind::Minus),
            b';' => self.make_token(TokenKind::Semicolon),
            b'?' => self.make_token(TokenKind::QuestionMark),
            b':' => self.make_token(TokenKind::Colon),
            b'=' if self.check_next(b'=') => self.make_token(TokenKind::EqualEqual),
            b'=' => self.make_token(TokenKind::Equal),
            b'!' if self.check_next(b'=') => self.make_token(TokenKind::BangEqual),
            b'!' => self.make_token(TokenKind::Bang),
            b'>' if self.check_next(b'=') => self.make_token(TokenKind::GreaterEqual),
            b'>' => self.make_token(TokenKind::Greater),
            b'<' if self.check_next(b'=') => self.make_token(TokenKind::LessEqual),
            b'<' => self.make_token(TokenKind::Less),
            b'/' => self.make_token(TokenKind::Slash),
            b'"' => self.parse_string(),
            byte if byte.is_ascii_digit() => self.parse_number(),
            byte if is_alpha(byte) => self.parse_identifier(),
            _ => self.error_token("Unexpected character: "),
        }
    }

    #[inline(always)]
    pub fn is_at_end(&self) -> bool {
        self.current == self.source.len()
    }

    #[inline(always)]
    fn peek(&self) -> u8 {
        self.source.as_bytes()[self.current]
    }

    fn peek_next(&self) -> u8 {
        if self.current < self.source.len() - 1 {
            self.source.as_bytes()[self.current + 1]
        } else {
            0
        }
    }

    fn check_next(&mut self, expected: u8) -> bool {
        if !self.is_at_end() && self.peek() == expected {
            self.current += 1;
            true
        } else {
            false
        }
    }

    #[inline(always)]
    fn advance(&mut self) -> u8 {
        let byte: u8 = self.peek();
        self.current += 1;
        byte
    }

    #[inline]
    fn make_token(&self, kind: TokenKind) -> Token<'a> {
        Token::new(
            kind,
            self.lexeme(),
            self.line
        )
    }

    #[inline]
    fn error_token(&self, msg: &'static str) -> Token<'a> {
        self.make_token(TokenKind::Error(msg))
    }

    fn skip_whitespace(&mut self) {
        while !self.is_at_end() {
            let byte = self.peek();
            match byte {
                b'\n' => {
                    self.line += 1;
                    self.current += 1;
                },
                b' ' | b'\r' | b'\t' => self.current += 1,
                b'/' if self.peek_next() == b'/' => {
                    while !self.is_at_end() && self.peek() != b'\n' {
                        self.current += 1;
                    }
                }
                b'/' if self.peek_next() == b'*' => {
                    self.current += 2;
                    match self.skip_comment() {
                        true => (),
                        false => eprintln!("[line {}] Error: Unterminated comment.", self.line)
                    }
                }
                _ => return
            }
        }
    }

    fn skip_comment(&mut self) -> bool {
        let mut level = 1;
        while !self.is_at_end() {
            match self.advance() {
                b'\n' => self.line += 1,
                b'/' if self.check_next(b'*') => {
                    level += 1;
                } 
                b'*' if self.check_next(b'/') => {
                    level -= 1;
                },
                _ => ()
            }
            if level == 0 {
                return true;
            }
        }
        false
    }

    fn parse_string(&mut self) -> Token<'a> {
        while !self.is_at_end() {
            let byte = self.advance();
            if byte != b'"' {
                continue;
            }
            return self.make_token(TokenKind::String)
        }

        Token::new(TokenKind::Error("Unterminated string."), "", self.line)
    }

    fn parse_number(&mut self) -> Token<'a> {
        while !self.is_at_end() && self.peek().is_ascii_digit() {
            self.current += 1;
        }

        if !self.is_at_end() && self.peek() == b'.' && self.peek_next().is_ascii_digit() {
            self.current += 2;
            while !self.is_at_end() && self.peek().is_ascii_digit() {
                self.current += 1;
            }
        }


        self.make_token(TokenKind::Number)
    }

    fn parse_identifier(&mut self) -> Token<'a> {
        while !self.is_at_end() && is_alphanumeric(self.peek()) {
            self.current += 1;
        }

        self.make_token(
            KEYWORDS
                .get(self.lexeme())
                .cloned()
                .unwrap_or(TokenKind::Identifier)
        )
    }

    #[inline(always)]
    fn lexeme(&self) -> &'a str {
        &self.source[self.start..self.current]
    }

    #[inline(always)]
    pub fn skip_shebang(&mut self) {
        if self.current != 0 {
            panic!("skip_shebang should be called at the beginning of the file.")
        }

        if self.source.starts_with("#!") {
            while !self.is_at_end() && self.advance() != b'\n' {}
            self.line += 1;
        }
    }
}

#[derive(Clone, Copy)]
pub struct Token<'a> {
    pub kind: TokenKind,
    lexeme: &'a str,
    line: usize,
}

impl<'a> Token<'a> {
    #[inline]
    fn new(kind: TokenKind, lexeme: &'a str, line: usize) -> Self {
        Self {
            kind,
            lexeme,
            line,
        }
    }
    
    #[inline(always)]
    pub fn get_lexeme(&self) -> &'a str {
        self.lexeme
    }

    #[inline(always)]
    pub fn get_line(&self) -> usize {
        self.line
    }
}

impl<'a> Debug for Token<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let value = match self.kind {
            //TODO
            TokenKind::Number => {
                match self.lexeme.parse::<f64>() {
                    Ok(val) => {
                        if val.fract() == 0.0 {
                            return write!(
                                f,
                                "{} {} {}",
                                self.kind,
                                self.lexeme,
                                format!("{:.1}", val)
                            )
                        } else {
                            self.lexeme
                        }
                    },
                    Err(_) => unreachable!(),
                }
            },
            TokenKind::String => &self.lexeme[1..self.lexeme.len() - 1],
            _ => "null"
        };
        
        write!(
            f,
            "{} {} {}",
            self.kind,
            self.lexeme,
            value
        )
    }
}

#[derive(PartialEq, Eq, Clone, Copy)]
pub enum TokenKind {
    // Single-character tokens.
    LeftParen,
    RightParen,
    LeftBrace,
    RightBrace,
    Comma,
    Dot,
    Minus,
    Plus,
    Semicolon,
    Slash,
    Star,
    QuestionMark,
    Colon,

    // One or two character tokens.
    Bang,
    BangEqual,
    Equal,
    EqualEqual,
    Greater,
    GreaterEqual,
    Less,
    LessEqual,

    // Literals.
    Identifier,
    String,
    Number,

    // Keywords.
    And,
    Class,
    Else,
    False,
    Fun,
    For,
    If,
    Nil,
    Or,
    Print,
    Return,
    Super,
    This,
    True,
    Var,
    While,
    
    EOF,

    Error(&'static str),
}

impl Display for TokenKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let result = match self {
            TokenKind::Var => "VAR",
            TokenKind::Identifier => "IDENTIFIER",
            TokenKind::String => "STRING",
            TokenKind::Number => "NUMBER",
            TokenKind::LeftParen => "LEFT_PAREN",
            TokenKind::RightParen => "RIGHT_PAREN",
            TokenKind::LeftBrace => "LEFT_BRACE",
            TokenKind::RightBrace => "RIGHT_BRACE",
            TokenKind::Comma => "COMMA",
            TokenKind::Dot => "DOT",
            TokenKind::Minus => "MINUS",
            TokenKind::Plus => "PLUS",
            TokenKind::Semicolon => "SEMICOLON",
            TokenKind::Slash => "SLASH",
            TokenKind::Star => "STAR",
            TokenKind::Bang => "BANG",
            TokenKind::BangEqual => "BANG_EQUAL",
            TokenKind::Equal => "EQUAL",
            TokenKind::EqualEqual => "EQUAL_EQUAL",
            TokenKind::Greater => "GREATER",
            TokenKind::GreaterEqual => "GREATER_EQUAL",
            TokenKind::Less => "LESS",
            TokenKind::LessEqual => "LESS_EQUAL",
            TokenKind::And => "AND",
            TokenKind::Class => "CLASS",
            TokenKind::Else => "ELSE",
            TokenKind::False => "FALSE",
            TokenKind::Fun => "FUN",
            TokenKind::For => "FOR",
            TokenKind::If => "IF",
            TokenKind::Nil => "NIL",
            TokenKind::Or => "OR",
            TokenKind::Print => "PRINT",
            TokenKind::Return => "RETURN",
            TokenKind::Super => "SUPER",
            TokenKind::This => "THIS",
            TokenKind::True => "TRUE",
            TokenKind::While => "WHILE",
            TokenKind::EOF => "EOF",
            TokenKind::QuestionMark => "QUESTION_MARK",
            TokenKind::Colon => "COLON",
            TokenKind::Error(_) => todo!(),
        };
        write!(f, "{}", result)
    }
}

#[inline(always)]
fn is_alpha(byte: u8) -> bool {
    matches!(byte, b'a'..=b'z' | b'A'..=b'Z' | b'_')
}

#[inline(always)]
fn is_alphanumeric(byte: u8) -> bool {
    matches!(byte, b'a'..=b'z' | b'A'..=b'Z' | b'0'..=b'9' | b'_')
}