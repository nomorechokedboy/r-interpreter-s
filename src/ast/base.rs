use super::statements::{
    BlockStatement, Bool, Call, ExpressionStatement, Function, Identifier, If, InfixExpression,
    IntegerLiteral, LetStatement, PrefixExpression, ReturnStatement,
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
    InfixExpression(InfixExpression),
    Bool(Bool),
    IfExpression(If),
    FunctionLiteral(Function),
    CallExpression(Call),
}

impl fmt::Display for Expression {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Expression::Identifier(expr) => write!(f, "{expr}",),
            Expression::IntegerLiteral(expr) => write!(f, "{expr}",),
            Expression::PrefixExpression(expr) => write!(f, "{expr}",),
            Expression::InfixExpression(expr) => write!(f, "{expr}",),
            Expression::Bool(expr) => write!(f, "{expr}"),
            Expression::IfExpression(expr) => write!(f, "{expr}"),
            Expression::FunctionLiteral(expr) => write!(f, "{expr}"),
            Expression::CallExpression(expr) => write!(f, "{expr}"),
        }
    }
}

#[derive(Clone, Debug)]
pub enum Statement {
    Let(LetStatement),
    Return(ReturnStatement),
    Expression(ExpressionStatement),
    // Block(BlockStatement),
}

impl Node for Statement {
    fn token_literal(&self) -> String {
        self.to_string()
    }
}

impl fmt::Display for Statement {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Statement::Let(stmt) => write!(f, "{stmt}"),
            Statement::Return(stmt) => write!(f, "{stmt}"),
            Statement::Expression(stmt) => write!(f, "{stmt}"),
            // Statement::Block(stmt) => write!(f, "{stmt}"),
        }
    }
}
