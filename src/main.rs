use std::env;
use std::fs;
use std::io::{self, Write};

mod scanner;
mod expr;
mod parser;
mod interpreter;
mod stmt;
mod environment;
mod resolver;

use interpreter::Interpreter;
use parser::Parser;
use resolver::Resolver;
use scanner::Token;
use scanner::TokenKind;

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() == 1 {
        println!("REPL is broken.");
        repl();
        std::process::exit(0);
    }

    let filename = &args[1];

    let file_contents = fs::read_to_string(filename).unwrap_or_else(|_| {
        eprintln!("Failed to read file {}", filename);
        String::new()
    });

    let mut parser = Parser::new(
        &file_contents
    );

    let mut stmts = Vec::with_capacity(32);
    while !parser.is_at_end() {
        match parser.parse() {
            Ok(stmt) => stmts.push(stmt),
            Err(_) => std::process::exit(65),
        }
    }

    let mut interpreter = Interpreter::new();

    let mut resolver = Resolver::new(&mut interpreter);
    match resolver.resolve_stmts(&stmts) {
        Ok(_) => (),
        Err(e) => {
            eprintln!("{}", e);
            std::process::exit(65)
        },
    };

    for stmt in stmts {
        match interpreter.interpret(&stmt) {
            Ok(_) => (),
            Err(_) => std::process::exit(70),
        }
    }
}

fn repl() {
    let mut input = String::new();
    let mut interpreter = Interpreter::new();
    loop {
        print!("> ");
        io::stdout().flush().unwrap();
        io::stdin()
            .read_line(&mut input)
            .expect("Unable to read line.");
        if input.is_empty() {
            break;
        }
        let mut parser = Parser::new(&input);
        match parser.parse() {
            Ok(stmt) => {
                let _ = interpreter.interpret(&stmt);
            },
            Err(_) => (),
        }
    }
}

fn report(token: &Token) {
    if let Token { kind: TokenKind::Error(msg), .. } = token {
        eprintln!("[line {}] Error: {}{}", token.get_line(), msg, token.get_lexeme())
    }
}