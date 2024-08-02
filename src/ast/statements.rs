use super::base::{Expression, Node};
use crate::token::Token;
use std::fmt::{self, Display};

#[derive(Debug, Clone)]
pub struct Identifier {
    pub token: Token,
}

impl Identifier {
    pub fn new(token: Token) -> Self {
        return Self { token };
    }

    pub fn value(&self) -> &str {
        match &self.token {
            Token::Ident(s) => s,
            _ => unreachable!("You should feel bad about yourself"),
        }
    }
}

impl Display for Identifier {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.value())
    }
}

impl Node for Identifier {
    fn token_literal(&self) -> String {
        self.token.to_string()
    }
}

#[derive(Debug)]
pub struct LetStatement {
    pub token: Token,
    pub name: Identifier,
    pub val: Option<Expression>,
}

impl LetStatement {
    pub fn new(token: Token, name: Identifier, val: Option<Expression>) -> Self {
        return Self { token, name, val };
    }
}

impl Node for LetStatement {
    fn token_literal(&self) -> String {
        self.token.to_string()
    }
}

impl fmt::Display for LetStatement {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let val = match &self.val {
            Some(t) => format!("{t}"),
            None => "".to_string(),
        };
        let let_stmt = format!(
            "{} {} = {};",
            self.token_literal(),
            self.name.to_string(),
            val
        );

        write!(f, "{}", let_stmt)
    }
}

#[derive(Debug)]
pub struct ReturnStatement {
    pub token: Token,
    pub val: Option<Expression>,
}

impl ReturnStatement {
    pub fn new(token: Token, val: Option<Expression>) -> Self {
        return Self { token, val };
    }
}

impl Node for ReturnStatement {
    fn token_literal(&self) -> String {
        self.token.to_string()
    }
}

impl fmt::Display for ReturnStatement {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let val = match &self.val {
            Some(t) => format!("{t}"),
            None => "".to_string(),
        };
        let return_stmt = format!("{} {};", self.token_literal(), val);

        write!(f, "{}", return_stmt)
    }
}

#[derive(Debug)]
pub struct ExpressionStatement {
    pub token: Token,
    pub expression: Expression,
}

impl Node for ExpressionStatement {
    fn token_literal(&self) -> String {
        self.token.to_string()
    }
}

impl fmt::Display for ExpressionStatement {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.expression.to_string())
    }
}
