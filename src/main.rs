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

use ast_printer::AstPrinter;
use interpreter::Interpreter;
use parser::Parser;
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
                match parser.expression() {
                    Ok(expr) => AstPrinter::print(&expr),
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

            let mut interpreter = Interpreter::new();

            while !parser.is_at_end() {
                match parser.parse() {
                    Ok(expr) => {
                        match interpreter.interpret(expr) {
                            Ok(_) => (),
                            Err(_) => exit_code = 70,
                        }
                    },
                    Err(_) => {
                        exit_code = 65;
                    },
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
                match interpreter.interpret(stmt) {
                    Ok(_) => (),
                    Err(e) => eprintln!("{}", e),
                }
            },
            Err(_) => (),
        }
        input.clear()
    }
}

fn report(token: &Token) {
    if let Token { kind: TokenKind::Error(msg), .. } = token {
        eprintln!("[line {}] Error: {}{}", token.get_line(), msg, token.get_lexeme())
    }
}