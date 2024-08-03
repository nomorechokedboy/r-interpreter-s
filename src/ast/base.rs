use super::statements::{
    ExpressionStatement, Identifier, IntegerLiteral, LetStatement, PrefixExpression,
    ReturnStatement,
};
use std::fmt::{self, Display};

pub trait Node: Display {
    fn token_literal(&self) -> String;
}

#[derive(Clone, Debug)]
pub enum Expression {
    Identifier(Identifier),
    IntegerLiteral(IntegerLiteral),
    PrefixExpression(PrefixExpression),
}

impl fmt::Display for Expression {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Expression::Identifier(ident) => write!(f, "{}", ident),
            Expression::IntegerLiteral(int_lit) => write!(f, "{}", int_lit),
            Expression::PrefixExpression(pre_exp) => write!(f, "{}", pre_exp),
        }
    }
}

#[derive(Debug)]
pub enum Statement {
    Let(LetStatement),
    Return(ReturnStatement),
    Expression(ExpressionStatement),
}

impl Node for Statement {
    fn token_literal(&self) -> String {
        self.to_string()
    }
}

impl fmt::Display for Statement {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Statement::Let(stmt) => write!(f, "{}", stmt),
            Statement::Return(stmt) => write!(f, "{}", stmt),
            Statement::Expression(stmt) => write!(f, "{}", stmt),
        }
    }
}
