use core::fmt;

use crate::scanner::{Token, TokenType};

pub enum Expression<'a> {
    Primary(Primary<'a>),
}

impl<'a> Expression<'a> {
    fn parse<'b>(token: &'b Token<'a>) -> Option<Self> {
        Primary::parse(token).map(Expression::Primary)
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
}

impl fmt::Display for Primary<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
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
    fn parse<'b>(token: &'b Token<'a>) -> Option<Self> {
        match token.token_type {
            TokenType::True => Some(Primary::True),
            TokenType::False => Some(Primary::False),
            TokenType::Nil => Some(Primary::Nil),
            TokenType::NumberLiteral => {
                Some(Primary::Number(token.literal.unwrap().unwrap_right()))
            }
            TokenType::StringLiteral => Some(Primary::String(token.literal.unwrap().unwrap_left())),
            _ => None,
        }
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
        for token in &self.tokens {
            if let Some(expr) = Expression::parse(token) {
                self.expr.push(expr);
            }
        }
    }
}
