use crate::{
    ast::base::{ASTNode, Expression, Statement},
    object::Object,
    token::Token,
};

pub fn eval(node: ASTNode) -> Option<Object> {
    match node {
        ASTNode::Expression(expr) => match expr {
            // Expression::Identifier(_) => todo!(),
            Expression::IntegerLiteral(expr) => Some(Object::Int64(expr.val)),
            Expression::PrefixExpression(expr) => {
                eval_prefix_expr(expr.token, eval(ASTNode::Expression(*expr.right))?)
            }
            Expression::InfixExpression(_) => todo!(),
            Expression::Bool(expr) => Some(Object::Bool(expr.value())),
            Expression::IfExpression(_) => todo!(),
            Expression::FunctionLiteral(_) => todo!(),
            Expression::CallExpression(_) => todo!(),
            _ => None,
        },
        ASTNode::Program(stmts) => eval_statements(stmts.statements),
        ASTNode::Statement(stmt) => match stmt {
            crate::ast::base::Statement::Expression(expr) => {
                eval(ASTNode::Expression(expr.expression))
            }
            _ => None,
        },
    }
}

fn eval_statements(stmts: Vec<Statement>) -> Option<Object> {
    for stmt in stmts {
        return eval(ASTNode::Statement(stmt));
    }

    None
}

fn eval_prefix_expr(token: Token, right: Object) -> Option<Object> {
    Some(match token {
        Token::Bang => eval_bang_expr(right),
        _ => Object::Null,
    })
}

fn eval_bang_expr(right: Object) -> Object {
    match right {
        Object::Bool(val) => match val {
            true => Object::Bool(false),
            false => Object::Bool(true),
        },
        Object::Null => Object::Bool(true),
        _ => Object::Bool(false),
    }
}
