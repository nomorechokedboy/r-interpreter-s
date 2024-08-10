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
            Expression::InfixExpression(expr) => Some(eval_infix_expr(
                expr.token,
                eval(ASTNode::Expression(*expr.left))?,
                eval(ASTNode::Expression(*expr.right))?,
            )),
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
        Token::Minus => eval_minus_prefix_operator_expr(right),
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

fn eval_minus_prefix_operator_expr(right: Object) -> Object {
    match right {
        Object::Int64(val) => Object::Int64(-val),
        _ => Object::Null,
    }
}

fn eval_infix_expr(token: Token, left: Object, right: Object) -> Object {
    match (left, right) {
        (Object::Int64(left), Object::Int64(right)) => eval_int_infix_expr(token, left, right),
        _ => Object::Null,
    }
}

fn eval_int_infix_expr(token: Token, left: i64, right: i64) -> Object {
    match token {
        Token::Plus => Object::Int64(left + right),
        Token::Minus => Object::Int64(left - right),
        Token::Asterisk => Object::Int64(left * right),
        Token::Slash => Object::Int64(left / right),
        _ => Object::Null,
    }
}
