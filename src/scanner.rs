use core::fmt;
use derive_more::Display;
use either::*;
use thiserror::Error;

#[derive(Display, Debug, PartialEq)]
pub enum TokenType {
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
    StringLiteral,
    #[display("NUMBER")]
    NumberLiteral,
    #[display("IDENTIFIER")]
    Identifier,
    // reserved words
    #[display("AND")]
    And,
    #[display("CLASS")]
    Class,
    #[display("ELSE")]
    Else,
    #[display("FALSE")]
    False,
    #[display("FOR")]
    For,
    #[display("FUN")]
    Fun,
    #[display("IF")]
    If,
    #[display("NIL")]
    Nil,
    #[display("OR")]
    Or,
    #[display("PRINT")]
    Print,
    #[display("RETURN")]
    Return,
    #[display("SUPER")]
    Super,
    #[display("THIS")]
    This,
    #[display("TRUE")]
    True,
    #[display("VAR")]
    Var,
    #[display("WHILE")]
    While,

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

    /// Return the length of the number literal
    /// We leave the parsing to the caller so that it knows the lexeme and its length
    /// It does not accept number literals like `.3`
    fn scan_number_literal(content: &str) -> Option<usize> {
        let mut dot_consumed = false;
        for (i, c) in content.char_indices() {
            if i == 0 && !c.is_numeric() {
                return None;
            }
            if c.is_numeric() || (c == '.' && !dot_consumed) {
                if c == '.' {
                    dot_consumed = true;
                }
            } else {
                return Some(i);
            }
        }
        Some(content.len())
    }

    /// Return the length of scanned identifier
    fn scan_identifier(content: &str) -> Option<usize> {
        for (i, c) in content.char_indices() {
            // an identifier starts with _ or letter
            if i == 0 && !(c.is_ascii_alphabetic() || c == '_') {
                return None;
            }
            if !(c.is_ascii_alphanumeric() || c == '_') {
                return Some(i);
            }
        }
        Some(content.len())
    }

    /// Return the token type and the length of the scanned reserved words
    /// This method is only called if `content` can be scanned as an identifier,
    /// this method checks whether the identifier is a reserved word
    fn scan_reserved_words(content: &str) -> Option<(Self, usize)> {
        match content {
            "and" => Some((Self::And, 3)),
            "class" => Some((Self::Class, 5)),
            "else" => Some((Self::Else, 4)),
            "false" => Some((Self::False, 5)),
            "for" => Some((Self::For, 3)),
            "fun" => Some((Self::Fun, 3)),
            "if" => Some((Self::If, 2)),
            "nil" => Some((Self::Nil, 3)),
            "or" => Some((Self::Or, 2)),
            "print" => Some((Self::Print, 5)),
            "return" => Some((Self::Return, 6)),
            "super" => Some((Self::Super, 5)),
            "this" => Some((Self::This, 4)),
            "true" => Some((Self::True, 4)),
            "var" => Some((Self::Var, 3)),
            "while" => Some((Self::While, 5)),
            _ => None,
        }
    }

    /// Greedy match on the longest token, construct and return the `Token`
    /// if no token is matched, return `Ok(None)`,
    /// if an error is encountered, propagate it to the caller.
    /// `line` and `offset` are updated during the scan.
    /// The order of matching is token type is the following:
    /// 1. String literal (starts with `"`)
    /// 2. Identifier (starts with [a-zA-Z_])
    /// 3. Reserved words (if it is already an identifier)
    /// 4. Number literal,
    /// 5. Double char token
    /// 6. Single char token
    fn scan<'a>(
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
                    Self::StringLiteral,
                    Some(&content[..len + 2]),                // include ""
                    Some(Either::Left(&content[1..len + 1])), // exclude "
                )));
            }
            Ok(None) => {
                if let Some(identifer_len) = Self::scan_identifier(content) {
                    // check whether it is exactly a reserved word
                    if let Some((reserved_type, token_len)) =
                        Self::scan_reserved_words(&content[..identifer_len])
                    {
                        *offset += identifer_len;
                        return Ok(Some(Token::new(
                            reserved_type,
                            Some(&content[..token_len]),
                            None,
                        )));
                    } else {
                        *offset += identifer_len;
                        return Ok(Some(Token::new(
                            Self::Identifier,
                            Some(&content[..identifer_len]),
                            None,
                        )));
                    }
                } else if let Some(number_len) = Self::scan_number_literal(content) {
                    *offset += number_len;
                    let number_lexeme = &content[..number_len];
                    let number = number_lexeme.parse::<f32>().unwrap();
                    return Ok(Some(Token::new(
                        Self::NumberLiteral,
                        Some(number_lexeme),
                        Some(Either::Right(number)),
                    )));
                } else if let Some(token_type) = Self::scan_double_char_token(content) {
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

#[derive(Debug)]
pub struct Token<'a> {
    pub token_type: TokenType,
    pub lexeme: Option<&'a str>,
    pub literal: Option<Either<&'a str, f32>>,
}

impl<'a> Token<'a> {
    fn new(
        token_type: TokenType,
        lexeme: Option<&'a str>,
        literal: Option<Either<&'a str, f32>>,
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
            match self.literal.as_ref().unwrap_or(&Either::Left("null")) {
                // display 1234 as 1234.0
                Either::Right(n) => {
                    if n.fract() == 0.0 {
                        format!("{:.1}", n)
                    } else {
                        n.to_string()
                    }
                }
                Either::Left(s) => s.to_string(),
            }
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
pub struct Scanner<'a> {
    pub tokens: Vec<Token<'a>>,
    /// Whether a scan error is encounted
    pub scan_error: bool,
}

impl<'a> Scanner<'a> {
    pub fn tokenize(&mut self, source: &'a str) {
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
            match TokenType::scan(&source[offset..], &mut line, &mut offset) {
                Err(e) => {
                    eprintln!("{}", e);
                    self.scan_error = true;
                }
                Ok(Some(token)) => {
                    self.tokens.push(token);
                }
                Ok(None) => {
                    let c = source.chars().nth(offset).unwrap();
                    if c == '\n' {
                        line += 1;
                    } else if !is_whitespace(c) {
                        eprintln!("{}", ScanError::UnexpectedChar(c, line));
                        self.scan_error = true;
                    }
                    offset += 1;
                }
            }
        }
        self.tokens.push(Token::default());
    }
}
