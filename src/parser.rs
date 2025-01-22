use crate::scanner::{Token, TokenType};
use core::fmt;
use either::Either;

pub enum Expression<'a> {
    Primary(Primary<'a>),
    Unary(Unary<'a>),
}

impl<'a> Expression<'a> {
    fn parse<'b>(tokens: &'b Vec<Token<'a>>, token_offset: &mut usize) -> Option<Self> {
        if *token_offset >= tokens.len() {
            return None;
        }
        // precedence: Unary > Primary
        Unary::parse(tokens, token_offset)
            .map(Expression::Unary)
            .or_else(|| Primary::parse(tokens, token_offset).map(Expression::Primary))
    }
}

impl fmt::Display for Expression<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Self::Primary(lit) => format!("{lit}"),
                Self::Unary(unary) => format!("{unary}"),
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
            TokenType::True => Some(Self::True),
            TokenType::False => Some(Self::False),
            TokenType::Nil => Some(Self::Nil),
            TokenType::NumberLiteral => Some(Self::Number(token.literal.unwrap().unwrap_right())),
            TokenType::StringLiteral => Some(Self::String(token.literal.unwrap().unwrap_left())),
            TokenType::LeftParen => {
                *token_offset += 1;
                Expression::parse(tokens, token_offset).map(|expr| {
                    assert_eq!(TokenType::RightParen, tokens[*token_offset].token_type);
                    Self::Group(Box::new(expr))
                })
            }
            _ => None,
        };
        *token_offset += 1;
        result
    }
}

pub enum Unary<'a> {
    Bang(Box<Either<Unary<'a>, Primary<'a>>>),
    Minus(Box<Either<Unary<'a>, Primary<'a>>>),
}

impl<'a> Unary<'a> {
    fn parse<'b>(tokens: &'b Vec<Token<'a>>, token_offset: &mut usize) -> Option<Self> {
        let token = &tokens[*token_offset];
        let result = match token.token_type {
            TokenType::Bang => {
                *token_offset += 1;
                Expression::parse(tokens, token_offset).map(|expr| match expr {
                    Expression::Unary(u) => Self::Bang(Box::new(Either::Left(u))),
                    Expression::Primary(p) => Self::Bang(Box::new(Either::Right(p))),
                })
            }
            TokenType::Minus => {
                *token_offset += 1;
                Expression::parse(tokens, token_offset).map(|expr| match expr {
                    Expression::Unary(u) => Self::Minus(Box::new(Either::Left(u))),
                    Expression::Primary(p) => Self::Minus(Box::new(Either::Right(p))),
                })
            }
            _ => None,
        };
        result
    }
}

impl fmt::Display for Unary<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Self::Bang(u) => format!("(! {})", u),
                Self::Minus(u) => format!("(- {})", u),
            }
        )
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
