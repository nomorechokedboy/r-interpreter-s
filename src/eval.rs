use crate::{
    ast::base::{ASTNode, Expression, Statement},
    object::Object,
};

pub fn eval(node: ASTNode) -> Option<Object> {
    match node {
        ASTNode::Expression(expr) => match expr {
            // Expression::Identifier(_) => todo!(),
            Expression::IntegerLiteral(expr) => Some(Object::Int64(expr.val)),
            Expression::PrefixExpression(_) => todo!(),
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
