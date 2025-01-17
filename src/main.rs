use std::env;
use std::fs::read_to_string;

mod scanner;

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() < 3 {
        eprintln!("Usage: {} tokenize <filename>", args[0]);
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
            let mut scanner = scanner::Scanner::default();

            let result = scanner.scan_tokens(&source);
            for token in &scanner.0 {
                println!("{token}");
            }
            if result.is_err() {
                std::process::exit(65);
            }
        }
        _ => {
            eprintln!("Unknown command: {}", command);
        }
    }
}
