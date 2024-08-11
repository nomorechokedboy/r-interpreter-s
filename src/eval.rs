use crate::{
    ast::{
        base::{ASTNode, Expression, Statement},
        program::Program,
        statements::{BlockStatement, If},
    },
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
            Expression::IfExpression(expr) => eval_if_expr(expr),
            Expression::FunctionLiteral(_) => todo!(),
            Expression::CallExpression(_) => todo!(),
            _ => None,
        },
        ASTNode::Program(stmts) => eval_program(stmts),
        ASTNode::Statement(stmt) => match stmt {
            Statement::Expression(stmt) => eval(ASTNode::Expression(stmt.expression)),
            Statement::Block(stmt) => eval_block_statement(stmt),
            Statement::Return(stmt) => Some(Object::Return(Box::new(eval(ASTNode::Expression(
                stmt.val?,
            ))?))),
            _ => None,
        },
    }
}

fn eval_statements(stmts: Vec<Statement>) -> Option<Object> {
    let mut res: Option<Object> = None;
    for stmt in stmts {
        res = eval(ASTNode::Statement(stmt));
        if let Some(Object::Return(val)) = res {
            return Some(*val);
        }
    }

    res
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
    match (&token, left, right) {
        (_, Object::Int64(left), Object::Int64(right)) => eval_int_infix_expr(token, left, right),
        (Token::Equal, Object::Bool(left), Object::Bool(right)) => Object::Bool(left == right),
        (Token::NotEqual, Object::Bool(left), Object::Bool(right)) => Object::Bool(left != right),
        _ => Object::Null,
    }
}

fn eval_int_infix_expr(token: Token, left: i64, right: i64) -> Object {
    match token {
        Token::Plus => Object::Int64(left + right),
        Token::Minus => Object::Int64(left - right),
        Token::Asterisk => Object::Int64(left * right),
        Token::Slash => Object::Int64(left / right),
        Token::LessThan => Object::Bool(left < right),
        Token::GreaterThan => Object::Bool(left > right),
        Token::Equal => Object::Bool(left == right),
        Token::NotEqual => Object::Bool(left != right),
        _ => Object::Null,
    }
}

fn eval_if_expr(i: If) -> Option<Object> {
    let cond = eval(ASTNode::Expression(*i.condition))?;
    if is_truthy(cond) {
        return eval(ASTNode::Statement(Statement::Block(i.consequence)));
    } else if let Some(else_block) = i.alternative {
        return eval(ASTNode::Statement(Statement::Block(else_block)));
    }

    Some(Object::Null)
}

fn is_truthy(obj: Object) -> bool {
    match obj {
        Object::Bool(val) => val,
        Object::Null => false,
        _ => true,
    }
}

fn eval_program(program: Program) -> Option<Object> {
    let mut res: Option<Object> = None;
    for stmt in program.statements {
        res = eval(ASTNode::Statement(stmt));
        if let Some(Object::Return(_)) = res {
            return res;
        }
    }

    res
}

fn eval_block_statement(block: BlockStatement) -> Option<Object> {
    let mut res: Option<Object> = None;
    for stmt in block.statements {
        res = eval(ASTNode::Statement(stmt));
        if let Some(Object::Return(_)) = res {
            return res;
        }
    }

    res
}
