use super::base::Statement;
use crate::ast::base::Node;
use std::{
    fmt::{self, Display},
    io::Write,
};

pub struct Program {
    pub statements: Vec<Statement>,
}

impl Display for Program {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut out = Vec::new();
        for statement in &self.statements {
            write!(out, "{}", statement).expect("should safely write to out");
        }

        write!(
            f,
            "{}",
            String::from_utf8(out).expect("should safely unwrap")
        )
    }
}

impl Node for Program {
    fn token_literal(&self) -> String {
        if !self.statements.is_empty() {
            return self.statements[0].token_literal();
        }

        String::new()
    }
}

impl Program {
    pub fn new(statements: Vec<Statement>) -> Self {
        return Self { statements };
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::{
        ast::{
            base::Expression,
            statements::{Identifier, LetStatement},
        },
        token::Token,
    };

    #[test]
    fn test_string() {
        let mut statements: Vec<Statement> = Vec::new();
        let name = "myVar".to_string();
        let val = "anotherVar".to_string();
        let let_stmt = LetStatement::new(
            Token::Let,
            Identifier::new(Token::Ident(name.clone())),
            Some(Expression::Identifier(Identifier::new(Token::Ident(
                val.clone(),
            )))),
        );
        statements.push(Statement::Let(let_stmt));

        let program = Program::new(statements);

        assert_eq!(program.to_string(), "let myVar = anotherVar;");
    }
}
