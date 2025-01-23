use std::env;
use std::fs::read_to_string;

use parser::Parser;
use scanner::Scanner;

mod parser;
mod scanner;

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() < 3 {
        eprintln!("Usage: {} [tokenize|parse] <filename>", args[0]);
        return;
    }

    let command = &args[1];
    let filename = &args[2];

    let Ok(source) = read_to_string(filename) else {
        // file not found
        println!("{}", scanner::Token::default());
        return;
    };

    match command.as_str() {
        "tokenize" => {
            let scanner = tokenize(&source);
            for token in &scanner.tokens {
                println!("{token}");
            }
            if scanner.scan_error {
                std::process::exit(65);
            }
        }
        "parse" => {
            let parser = parse(&source);
            for expr in &parser.exprs {
                println!("{expr}");
            }
        }
        _ => {
            eprintln!("Unknown command: {}", command);
        }
    }
}

fn tokenize(source: &str) -> Scanner {
    let mut scanner = scanner::Scanner::default();
    scanner.tokenize(source);
    scanner
}

fn parse(source: &str) -> Parser {
    let scanner = tokenize(source);
    if scanner.scan_error {
        std::process::exit(65);
    }
    // consume the scanner
    let mut parser = parser::Parser::new(scanner.tokens);
    parser.parse();
    parser
}
