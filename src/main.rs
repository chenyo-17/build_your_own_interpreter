use core::fmt;
use derive_more::Display;
use either::*;
use std::env;
use std::fs::read_to_string;
use std::path::{Path, PathBuf};
use thiserror::Error;

#[derive(Display)]
enum TokenType {
    #[display("LEFT_PAREN")]
    LeftParen,
    #[display("RIGHT_PAREN")]
    RightParen,
    #[display("LEFT_BRACE")]
    LeftBrace,
    #[display("RIGHT_BRACE")]
    RightBrace,
    #[display("STAR")]
    Star,
    #[display("DOT")]
    Dot,
    #[display("PLUS")]
    Plus,
    #[display("MINUS")]
    Minus,
    #[display("COMMA")]
    Comma,
    #[display("SEMICOLON")]
    Semicolon,
    #[display("EOF")]
    Eof,
}

impl TokenType {
    // TODO: use a hashmap to store the lexeme and its name
    fn scan_single_char_token(token: char) -> Option<Self> {
        match token {
            '(' => Some(Self::LeftParen),
            ')' => Some(Self::RightParen),
            '{' => Some(Self::LeftBrace),
            '}' => Some(Self::RightBrace),
            '*' => Some(Self::Star),
            '.' => Some(Self::Dot),
            '+' => Some(Self::Plus),
            '-' => Some(Self::Minus),
            ',' => Some(Self::Comma),
            ';' => Some(Self::Semicolon),
            _ => None,
        }
    }
}

struct Token {
    token_type: TokenType,
    lexeme: Option<String>,
    literal: Option<Either<String, i32>>,
}

impl Token {
    fn new(
        token_type: TokenType,
        lexeme: Option<String>,
        literal: Option<Either<String, i32>>,
    ) -> Self {
        Self {
            token_type,
            lexeme,
            literal,
        }
    }
}

impl Default for Token {
    fn default() -> Self {
        Self {
            token_type: TokenType::Eof,
            lexeme: None,
            literal: None,
        }
    }
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{} {} {}",
            self.token_type,
            self.lexeme.as_ref().unwrap_or(&String::new()),
            self.literal
                .as_ref()
                .unwrap_or(&Either::Left("null".to_string()))
        )
    }
}

#[derive(Error, Debug)]
enum ScanError {
    #[error("Unknown token type {0}")]
    UnknownToken(String),
}

struct Scanner {
    path: PathBuf,
    tokens: Vec<Token>,
}

impl Scanner {
    fn new(path: impl AsRef<Path>) -> Self {
        Self {
            path: path.as_ref().into(),
            tokens: vec![],
        }
    }

    fn scan_tokens(&mut self) -> anyhow::Result<()> {
        let Ok(source) = read_to_string(&self.path) else {
            self.tokens.push(Token::default());
            return Ok(());
        };
        for content in source.lines() {
            // TODO: only scan single character token
            for c in content.chars() {
                let Some(token_type) = TokenType::scan_single_char_token(c) else {
                    return Err(ScanError::UnknownToken(c.to_string()).into());
                };
                self.tokens
                    .push(Token::new(token_type, Some(c.to_string()), None));
            }
        }
        self.tokens.push(Token::new(TokenType::Eof, None, None));
        Ok(())
    }
}

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() < 3 {
        eprintln!("Usage: {} tokenize <filename>", args[0]);
        return;
    }

    let command = &args[1];
    let filename = &args[2];

    match command.as_str() {
        "tokenize" => {
            // You can use print statements as follows for debugging, they'll be visible when running tests.
            // eprintln!("Logs from your program will appear here!");

            let mut scanner = Scanner::new(filename);
            if let Err(e) = scanner.scan_tokens() {
                eprintln!("{e}");
            }

            for token in scanner.tokens {
                println!("{token}");
            }
        }
        _ => {
            eprintln!("Unknown command: {}", command);
        }
    }
}
