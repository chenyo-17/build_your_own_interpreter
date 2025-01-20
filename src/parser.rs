use core::fmt;

use crate::scanner::{Token, TokenType};

pub enum Expression<'a> {
    Primary(Primary<'a>),
}

impl<'a> Expression<'a> {
    fn parse<'b>(tokens: &'b Vec<Token<'a>>, token_offset: &mut usize) -> Option<Self> {
        if *token_offset >= tokens.len() {
            return None;
        }
        Primary::parse(tokens, token_offset).map(Expression::Primary)
        // if let Some(lit) = Literal::parse(token) {
        //     Some(Expression::Literal(lit))
        // } else {
        //     None
        // }
    }
}

impl fmt::Display for Expression<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Self::Primary(lit) => lit,
            }
        )
    }
}

pub enum Primary<'a> {
    Number(f32),
    String(&'a str),
    Nil,
    True,
    False,
    Group(Box<Expression<'a>>),
}

impl fmt::Display for Primary<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Self::Group(e) => format!("(group {})", e),
                Self::Number(n) => {
                    if n.fract() == 0.0 {
                        format!("{:.1}", n)
                    } else {
                        n.to_string()
                    }
                }
                Self::String(s) => s.to_string(),
                Self::True => "true".to_string(),
                Self::False => "false".to_string(),
                Self::Nil => "nil".to_string(),
            }
        )
    }
}

impl<'a> Primary<'a> {
    fn parse<'b>(tokens: &'b Vec<Token<'a>>, token_offset: &mut usize) -> Option<Self> {
        let token = &tokens[*token_offset];
        let result = match token.token_type {
            TokenType::True => Some(Primary::True),
            TokenType::False => Some(Primary::False),
            TokenType::Nil => Some(Primary::Nil),
            TokenType::NumberLiteral => {
                Some(Primary::Number(token.literal.unwrap().unwrap_right()))
            }
            TokenType::StringLiteral => Some(Primary::String(token.literal.unwrap().unwrap_left())),
            TokenType::LeftParen => {
                *token_offset += 1;
                if let Some(group_expr) = Expression::parse(tokens, token_offset) {
                    assert_eq!(TokenType::RightParen, tokens[*token_offset].token_type);
                    Some(Primary::Group(Box::new(group_expr)))
                } else {
                    None
                }
            }
            _ => None,
        };
        *token_offset += 1;
        result
    }
}

pub struct Parser<'a> {
    tokens: Vec<Token<'a>>,
    pub expr: Vec<Expression<'a>>,
}

impl<'a> Parser<'a> {
    pub fn new(tokens: Vec<Token<'a>>) -> Self {
        Parser {
            tokens,
            expr: vec![],
        }
    }

    pub fn parse(&mut self) {
        let mut token_offset = 0;
        while token_offset < self.tokens.len() {
            if let Some(expr) = Expression::parse(&self.tokens, &mut token_offset) {
                self.expr.push(expr);
            }
        }
    }
}
