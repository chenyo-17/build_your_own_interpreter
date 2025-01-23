use crate::scanner::{Token, TokenType};
use core::fmt;
use either::Either;

#[derive(Debug)]
pub enum Expression<'a> {
    Primary(Primary<'a>),
    Unary(Unary<'a>),
    Binary(Binary<'a>),
}

impl<'a> Expression<'a> {
    fn parse<'b, 'c>(
        exprs: &'c mut Vec<Expression<'a>>,
        tokens: &'b Vec<Token<'a>>,
        token_offset: &mut usize,
    ) -> Option<Self> {
        if *token_offset >= tokens.len() {
            return None;
        }
        // Precedence: Primary > Unary > Binary
        if let Some(expr) = Primary::parse(exprs, tokens, token_offset) {
            return Some(Expression::Primary(expr));
        }
        if let Some(expr) = Unary::parse(exprs, tokens, token_offset) {
            return Some(Expression::Unary(expr));
        }
        if let Some(expr) = Binary::parse(exprs, tokens, token_offset) {
            return Some(Expression::Binary(expr));
        }
        *token_offset += 1;
        None
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
                Self::Binary(binary) => format!("{binary}"),
            }
        )
    }
}

#[derive(Debug)]
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
    fn parse<'b, 'c>(
        exprs: &'c mut Vec<Expression<'a>>,
        tokens: &'b Vec<Token<'a>>,
        token_offset: &mut usize,
    ) -> Option<Self> {
        let token = &tokens[*token_offset];
        let result = match token.token_type {
            TokenType::True => Some(Self::True),
            TokenType::False => Some(Self::False),
            TokenType::Nil => Some(Self::Nil),
            TokenType::NumberLiteral => Some(Self::Number(token.literal.unwrap().unwrap_right())),
            TokenType::StringLiteral => Some(Self::String(token.literal.unwrap().unwrap_left())),
            TokenType::LeftParen => {
                *token_offset += 1;
                while tokens[*token_offset].token_type != TokenType::RightParen {
                    let expr = Expression::parse(exprs, tokens, token_offset)?;
                    // push intermetidate new exprs
                    exprs.push(expr);
                }
                let grouped_expr = exprs.pop()?;
                Some(Self::Group(Box::new(grouped_expr)))
            }
            _ => None,
        };
        if result.is_some() {
            // advance to next token
            *token_offset += 1;
        }
        result
    }
}

#[derive(Debug)]
pub enum Unary<'a> {
    Bang(Box<Either<Unary<'a>, Primary<'a>>>),
    Minus(Box<Either<Unary<'a>, Primary<'a>>>),
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

impl<'a> Unary<'a> {
    fn parse<'b, 'c>(
        exprs: &'c mut Vec<Expression<'a>>,
        tokens: &'b Vec<Token<'a>>,
        token_offset: &mut usize,
    ) -> Option<Self> {
        let token = &tokens[*token_offset];
        let result = match token.token_type {
            TokenType::Bang => {
                *token_offset += 1;
                Expression::parse(exprs, tokens, token_offset).and_then(|expr| match expr {
                    Expression::Unary(u) => Some(Self::Bang(Box::new(Either::Left(u)))),
                    Expression::Primary(p) => Some(Self::Bang(Box::new(Either::Right(p)))),
                    _ => None,
                })
            }
            TokenType::Minus => {
                *token_offset += 1;
                Expression::parse(exprs, tokens, token_offset).and_then(|expr| match expr {
                    Expression::Unary(u) => Some(Self::Minus(Box::new(Either::Left(u)))),
                    Expression::Primary(p) => Some(Self::Minus(Box::new(Either::Right(p)))),
                    _ => None,
                })
            }
            _ => None,
        };
        // we don't increase the offset now, because the last parsed token is always a Primary,
        // which has advanced itself
        result
    }
}

#[derive(Debug)]
pub enum Binary<'a> {
    Slash(Box<Expression<'a>>, Box<Expression<'a>>),
    Star(Box<Expression<'a>>, Box<Expression<'a>>),
    // Minus(Box<Expression<'a>>, Box<Expression<'a>>),
    // Plus(Box<Expression<'a>>, Box<Expression<'a>>),
}

impl fmt::Display for Binary<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Self::Slash(l, r) => format!("(/ {} {})", l, r),
                Self::Star(l, r) => format!("(* {} {})", l, r),
            }
        )
    }
}

impl<'a> Binary<'a> {
    fn parse<'b, 'c>(
        exprs: &'c mut Vec<Expression<'a>>,
        tokens: &'b Vec<Token<'a>>,
        token_offset: &mut usize,
    ) -> Option<Self> {
        let token = &tokens[*token_offset];
        let result = match token.token_type {
            TokenType::Slash => {
                *token_offset += 1;
                // parse right operand
                let right = Expression::parse(exprs, tokens, token_offset)?;
                // pop left operand
                let left = exprs.pop()?;
                Some(Self::Slash(Box::new(left), Box::new(right)))
            }
            TokenType::Star => {
                *token_offset += 1;
                // parse right operand
                let right = Expression::parse(exprs, tokens, token_offset)?;
                // pop left operand
                let left = exprs.pop()?;
                Some(Self::Star(Box::new(left), Box::new(right)))
            }
            _ => None,
        };
        result
    }
}

#[derive(Debug)]
pub struct Parser<'a> {
    tokens: Vec<Token<'a>>,
    pub exprs: Vec<Expression<'a>>,
}

impl<'a> Parser<'a> {
    pub fn new(tokens: Vec<Token<'a>>) -> Self {
        Parser {
            tokens,
            exprs: vec![],
        }
    }

    pub fn parse(&mut self) {
        let mut token_offset = 0;
        while token_offset < self.tokens.len() {
            if let Some(expr) = Expression::parse(&mut self.exprs, &self.tokens, &mut token_offset)
            {
                self.exprs.push(expr);
            }
        }
    }
}
