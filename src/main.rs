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
    #[display("EQUAL")]
    Equal,
    #[display("EQUAL_EQUAL")]
    EuqalEqual,
    #[display("BANG")]
    Bang,
    #[display("BANG_EQUAL")]
    BangEqual,
    #[display("LESS")]
    Less,
    #[display("LESS_EQUAL")]
    LessEqual,
    #[display("GREATER")]
    Greater,
    #[display("GREATER_EQUAL")]
    GreaterEqual,
    #[display("SLASH")]
    Slash,
    #[display("EOF")]
    Eof,
}

impl TokenType {
    // TODO: whether using a hashmap to store the lexeme and its name is better?
    fn scan_single_char_token(content: &str) -> Option<Self> {
        if content.is_empty() {
            return None;
        }
        match &content[..1] {
            "(" => Some(Self::LeftParen),
            ")" => Some(Self::RightParen),
            "{" => Some(Self::LeftBrace),
            "}" => Some(Self::RightBrace),
            "*" => Some(Self::Star),
            "." => Some(Self::Dot),
            "+" => Some(Self::Plus),
            "-" => Some(Self::Minus),
            "," => Some(Self::Comma),
            ";" => Some(Self::Semicolon),
            "=" => Some(Self::Equal),
            "!" => Some(Self::Bang),
            "<" => Some(Self::Less),
            ">" => Some(Self::Greater),
            "/" => Some(Self::Slash),
            _ => None,
        }
    }

    fn scan_double_char_token(content: &str) -> Option<Self> {
        if content.len() < 2 {
            return None;
        }
        match &content[..2] {
            "==" => Some(Self::EuqalEqual),
            "!=" => Some(Self::BangEqual),
            "<=" => Some(Self::LessEqual),
            ">=" => Some(Self::GreaterEqual),
            _ => None,
        }
    }

    /// Greedy match on the longest token, return the matched token type
    /// and the token length
    fn scan_token(content: &str) -> Option<(Self, usize)> {
        if let Some(token_type) = Self::scan_double_char_token(content) {
            return Some((token_type, 2));
        } else if let Some(token_type) = Self::scan_single_char_token(content) {
            return Some((token_type, 1));
        }
        None
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
    #[error("[line {1}] Error: Unexpected character: {0}")]
    UnexpectedChar(char, usize),
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
        fn is_comment(content: &str) -> bool {
            if content.len() < 2 {
                return false;
            }
            if &content[..2] == "//" {
                return true;
            }
            false
        }

        fn is_whitespace(c: char) -> bool {
            match c {
                ' ' | '\r' | '\t' => true,
                _ => false,
            }
        }

        let Ok(source) = read_to_string(&self.path) else {
            self.tokens.push(Token::default());
            return Ok(());
        };
        let mut result = Ok(());
        for (line, content) in source.lines().enumerate().map(|(i, x)| (i + 1, x)) {
            let mut offset = 0;
            while offset < content.len() {
                if is_comment(&content[offset..]) {
                    // skip the whole line
                    break;
                }
                match TokenType::scan_token(&content[offset..]) {
                    Some((token_type, token_length)) => {
                        self.tokens.push(Token::new(
                            token_type,
                            Some(String::from(&content[offset..offset + token_length])),
                            None,
                        ));
                        offset += token_length;
                    }
                    None => {
                        let c = content.chars().nth(offset).unwrap();
                        // the newline \n is already handled by `.lines()`
                        if !is_whitespace(c) {
                            eprintln!("{}", ScanError::UnexpectedChar(c, line));
                            result = Err(ScanError::UnexpectedChar(c, line).into());
                        }
                        offset += 1;
                    }
                }
            }
        }
        self.tokens.push(Token::new(TokenType::Eof, None, None));
        result
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
            let mut scanner = Scanner::new(filename);

            let result = scanner.scan_tokens();
            for token in &scanner.tokens {
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
