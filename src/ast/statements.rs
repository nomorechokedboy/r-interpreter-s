use super::base::{Expression, Node};
use crate::token::Token;
use std::fmt::{self, Display};

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

impl Display for LetStatement {
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

impl Display for ReturnStatement {
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

impl Display for ExpressionStatement {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.expression.to_string())
    }
}

#[derive(Clone, Debug)]
pub struct Identifier {
    pub token: Token,
}

impl Identifier {
    pub fn new(token: Token) -> Self {
        match token {
            Token::Ident(_) => Self { token },
            _ => panic!(
                "Identifier must be created with Token::Ident, got {:?}",
                token
            ),
        }
    }

    pub fn value(&self) -> &str {
        match &self.token {
            Token::Ident(s) => s,
            _ => panic!("Expected Token::Ident, got {:#?}", self.token),
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

#[derive(Clone, Debug, PartialEq)]
pub struct IntegerLiteral {
    pub token: Token,
    pub val: i64,
}

impl IntegerLiteral {
    pub fn new(token: Token, val: i64) -> Self {
        Self { token, val }
    }
}

impl Node for IntegerLiteral {
    fn token_literal(&self) -> String {
        self.token.to_string()
    }
}

impl Display for IntegerLiteral {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.val)
    }
}

#[derive(Clone, Debug)]
pub struct PrefixExpression {
    pub token: Token,
    pub operator: String,
    pub right: Box<Expression>,
}

impl PrefixExpression {
    pub fn new(token: Token, operator: String, right: Box<Expression>) -> Self {
        Self {
            token,
            operator,
            right,
        }
    }
}

impl Node for PrefixExpression {
    fn token_literal(&self) -> String {
        self.token.to_string()
    }
}

impl Display for PrefixExpression {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "({}{})", self.operator, self.right.to_string())
    }
}
