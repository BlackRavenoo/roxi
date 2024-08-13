use std::env;
use std::fs;
use std::io::{self, Write};

mod scanner;
mod expr;
mod ast_printer;
mod parser;
mod interpreter;
mod stmt;
mod environment;
mod resolver;

use ast_printer::AstPrinter;
use interpreter::Interpreter;
use parser::Parser;
use resolver::Resolver;
use scanner::Scanner;
use scanner::Token;
use scanner::TokenKind;

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() < 3 {
        if args.len() == 1 {
            repl();
            std::process::exit(0)
        }
        writeln!(io::stderr(), "Usage: {} tokenize <filename>", args[0]).unwrap();
        std::process::exit(64);
    }

    let command = &args[1];
    let filename = &args[2];

    let mut exit_code = 0;

    match command.as_str() {
        "tokenize" => {
            let file_contents = fs::read_to_string(filename).unwrap_or_else(|_| {
                writeln!(io::stderr(), "Failed to read file {}", filename).unwrap();
                String::new()
            });

            let mut scanner = Scanner::new(
                &file_contents
            );

            loop {
                let token = scanner.scan_token();
                match &token {
                    Token { kind: TokenKind::Error(_), ..} => {
                        report(&token);
                        exit_code = 65;
                    },
                    _ =>  println!("{:?}", token)
                }
                if token.kind == TokenKind::EOF {
                    break;
                }
            }
        }
        "parse" => {
            let file_contents = fs::read_to_string(filename).unwrap_or_else(|_| {
                writeln!(io::stderr(), "Failed to read file {}", filename).unwrap();
                String::new()
            });

            let mut parser = Parser::new(
                &file_contents
            );

            while !parser.is_at_end() {
                match parser.parse() {
                    Ok(stmt) => AstPrinter::print(&stmt),
                    Err(e) => {
                        eprintln!("{}", e);
                        exit_code = 65;
                        break;
                    },
                }
            }
        }
        "evaluate" => {
            let file_contents = fs::read_to_string(filename).unwrap_or_else(|_| {
                writeln!(io::stderr(), "Failed to read file {}", filename).unwrap();
                String::new()
            });

            let mut parser = Parser::new(
                &file_contents
            );

            let mut stmts = Vec::with_capacity(32);
            while !parser.is_at_end() {
                match parser.parse() {
                    Ok(stmt) => stmts.push(stmt),
                    Err(_) => exit_code = 65,
                }
            }

            if exit_code != 0 {
                std::process::exit(exit_code)
            }

            let mut interpreter = Interpreter::new();
            {
                let mut resolver = Resolver::new(&mut interpreter);
                match resolver.resolve_stmts(&stmts) {
                    Ok(_) => (),
                    Err(e) => {
                        eprintln!("{}", e);
                        std::process::exit(65)
                    },
                };
            }

            for stmt in stmts {
                match interpreter.interpret(&stmt) {
                    Ok(_) => (),
                    Err(_) => std::process::exit(70),
                }
            }
        }
        _ => {
            writeln!(io::stderr(), "Unknown command: {}", command).unwrap();
            std::process::exit(64);
        }
    }

    if exit_code != 0 {
        std::process::exit(exit_code);
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