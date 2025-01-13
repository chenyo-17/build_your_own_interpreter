use core::fmt;
use derive_more::Display;
use either::*;
use std::env;
use std::fs::read_to_string;
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
    #[display("STRING")]
    String_,
    #[display("EOF")]
    Eof,
}

impl TokenType {
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

    /// If no left `"` is found, returns `Ok(None)`;
    /// If a pair of `"` is found, returns the string literal length (both " are exclusived);
    /// If the right `"` is not found, return the error.
    /// The current line is updated accordingly when a newline is found in the string literal.
    fn scan_string_literal(content: &str, line: &mut usize) -> anyhow::Result<Option<usize>> {
        if content.is_empty() || &content[..1] != "\"" {
            return Ok(None);
        }
        for (i, c) in content[1..].char_indices() {
            if c == '\n' {
                *line += 1;
            }
            if c == '"' {
                return Ok(Some(i)); // " exclusive
            }
        }
        Err(ScanError::UnterminedString(*line).into())
    }

    /// Greedy match on the longest token, construct and return the `Token`
    /// if no token is matched, return `Ok(None)`,
    /// if an error is encountered, propagate it to the caller.
    /// `line` and `offset` are updated during the scan.
    fn scan_token<'a>(
        content: &'a str,
        line: &mut usize,
        offset: &mut usize,
    ) -> anyhow::Result<Option<Token<'a>>> {
        match Self::scan_string_literal(content, line) {
            Err(e) => {
                *offset += content.len(); // has scanned the entire file
                return Err(e);
            }
            Ok(Some(len)) => {
                *offset += len + 2; // include ""
                return Ok(Some(Token::new(
                    Self::String_,
                    Some(&content[..len + 2]),                // include ""
                    Some(Either::Left(&content[1..len + 1])), // exclude "
                )));
            }
            Ok(None) => {
                if let Some(token_type) = Self::scan_double_char_token(content) {
                    *offset += 2;
                    return Ok(Some(Token::new(token_type, Some(&content[..2]), None)));
                } else if let Some(token_type) = Self::scan_single_char_token(content) {
                    *offset += 1;
                    return Ok(Some(Token::new(token_type, Some(&content[..1]), None)));
                }
            }
        }
        Ok(None)
    }
}

struct Token<'a> {
    token_type: TokenType,
    lexeme: Option<&'a str>,
    literal: Option<Either<&'a str, i32>>,
}

impl<'a> Token<'a> {
    fn new(
        token_type: TokenType,
        lexeme: Option<&'a str>,
        literal: Option<Either<&'a str, i32>>,
    ) -> Self {
        Self {
            token_type,
            lexeme,
            literal,
        }
    }
}

impl Default for Token<'_> {
    fn default() -> Self {
        Self {
            token_type: TokenType::Eof,
            lexeme: None,
            literal: None,
        }
    }
}

impl fmt::Display for Token<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{} {} {}",
            self.token_type,
            self.lexeme.as_ref().unwrap_or(&""),
            self.literal.as_ref().unwrap_or(&Either::Left("null"))
        )
    }
}

#[derive(Error, Debug)]
enum ScanError {
    #[error("[line {1}] Error: Unexpected character: {0}")]
    UnexpectedChar(char, usize),
    #[error("[line {0}] Error: Unterminated string.")]
    UnterminedString(usize),
}

#[derive(Default)]
struct Scanner<'a>(Vec<Token<'a>>);

impl<'a> Scanner<'a> {
    fn scan_tokens(&mut self, source: &'a str) -> anyhow::Result<()> {
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
            matches!(c, ' ' | '\r' | '\t')
        }

        let mut result = Ok(());
        let (mut line, mut offset) = (1, 0);
        while offset < source.len() {
            if is_comment(&source[offset..]) {
                // skip the whole line
                offset = source[offset..]
                    .find('\n')
                    .map(|p| offset + p)
                    .unwrap_or(source.len());
                continue;
            }
            match TokenType::scan_token(&source[offset..], &mut line, &mut offset) {
                Err(e) => {
                    eprintln!("{}", e);
                    result = Err(e);
                }
                Ok(Some(token)) => {
                    self.0.push(token);
                }
                Ok(None) => {
                    let c = source.chars().nth(offset).unwrap();
                    if c == '\n' {
                        line += 1;
                    } else if !is_whitespace(c) {
                        eprintln!("{}", ScanError::UnexpectedChar(c, line));
                        result = Err(ScanError::UnexpectedChar(c, line).into());
                    }
                    offset += 1;
                }
            }
        }
        self.0.push(Token::default());
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

    let Ok(source) = read_to_string(filename) else {
        // file not found
        println!("{}", Token::default());
        return;
    };

    match command.as_str() {
        "tokenize" => {
            let mut scanner = Scanner::default();

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
