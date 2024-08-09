use crate::{
    ast::base::{ASTNode, Expression, Statement},
    object::Object,
};

pub fn eval(node: ASTNode) -> Option<Object> {
    match node {
        ASTNode::Expression(expr) => match expr {
            // Expression::Identifier(_) => todo!(),
            Expression::IntegerLiteral(int) => Some(Object::Int64(int.val)),
            /* Expression::PrefixExpression(_) => todo!(),
            Expression::InfixExpression(_) => todo!(),
            Expression::Bool(_) => todo!(),
            Expression::IfExpression(_) => todo!(),
            Expression::FunctionLiteral(_) => todo!(),
            Expression::CallExpression(_) => todo!(), */
            _ => todo!(),
        },
        ASTNode::Program(stmts) => eval_statements(stmts.statements),
        ASTNode::Statement(stmt) => match stmt {
            crate::ast::base::Statement::Expression(expr) => {
                eval(ASTNode::Expression(expr.expression))
            }
            _ => todo!(),
        },
    }
}

fn eval_statements(stmts: Vec<Statement>) -> Option<Object> {
    for stmt in stmts {
        return eval(ASTNode::Statement(stmt));
    }

    None
}
