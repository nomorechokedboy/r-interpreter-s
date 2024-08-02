use crate::{
    ast::{
        base::{Expression, Statement},
        program::Program,
        statements::{ExpressionStatement, Identifier, LetStatement, ReturnStatement},
    },
    lexer::Lexer,
    token::Token,
};
use std::collections::HashMap;

type PrefixParseFn = for<'a> fn(&'a mut Parser) -> Expression;
type InfixParseFn = for<'a> fn(&'a mut Parser, Expression) -> Expression;

pub struct Parser {
    cur_token: Token,
    errs: Vec<String>,
    lexer: Lexer,
    peek_token: Token,
    prefix_parse_fns: HashMap<Token, PrefixParseFn>,
    infix_parse_fns: HashMap<Token, InfixParseFn>,
}

#[derive(Debug)]
enum Precedence {
    Lowest,
    Equals,
    LessGreater,
    Sum,
    Product,
    Prefix,
    Call,
}

impl Parser {
    pub fn new(lexer: Lexer) -> Self {
        let mut parser = Self {
            cur_token: Token::Illegal,
            errs: vec![],
            lexer,
            peek_token: Token::Illegal,
            prefix_parse_fns: HashMap::new(),
            infix_parse_fns: HashMap::new(),
        };
        parser.next_token();
        parser.next_token();
        parser.register_prefix(Token::Ident(String::new()), Parser::parse_identifier);

        parser
    }

    pub fn errs(&self) -> Vec<String> {
        self.errs.clone()
    }

    pub fn parse_program(&mut self) -> Program {
        let mut program = Program::new(Vec::new());

        while self.cur_token != Token::Eof {
            let maybe_stmt = self.parse_statement();
            if let Some(stmt) = maybe_stmt {
                program.statements.push(stmt);
            }

            self.next_token();
        }

        program
    }

    fn next_token(&mut self) {
        self.cur_token = self.peek_token.clone();
        self.peek_token = self.lexer.next_token()
    }

    fn parse_statement(&mut self) -> Option<Statement> {
        match self.cur_token {
            Token::Let => self.parse_let_statement(),
            Token::Return => self.parse_return_statement(),
            _ => self.parse_expression_statement(),
        }
    }

    fn parse_let_statement(&mut self) -> Option<Statement> {
        let token = self.cur_token.clone();

        if !self.expect_peek(&Token::Ident(String::new())) {
            return None;
        }

        let name = Identifier::new(self.cur_token.clone());

        if !self.expect_peek(&Token::Assign) {
            return None;
        }

        while !self.cur_token_is(&Token::Semicolon) {
            self.next_token();
        }

        let stmt = LetStatement {
            token,
            name,
            val: None,
        };

        Some(Statement::Let(stmt))
    }

    fn parse_return_statement(&mut self) -> Option<Statement> {
        let token = self.cur_token.clone();
        self.next_token();

        while !self.cur_token_is(&Token::Semicolon) {
            self.next_token();
        }

        Some(Statement::Return(ReturnStatement::new(token, None)))
    }

    fn parse_expression_statement(&mut self) -> Option<Statement> {
        let (token, expression) = (
            self.cur_token.clone(),
            self.parse_expression(Precedence::Lowest)?,
        );

        if self.peek_token_is(&Token::Semicolon) {
            self.next_token();
        }

        Some(Statement::Expression(ExpressionStatement {
            token,
            expression,
        }))
    }

    fn parse_expression(&mut self, precedence: Precedence) -> Option<Expression> {
        let prefix = self.prefix_parse_fns.get(&Token::Ident(String::new()))?;
        Some(prefix(self))
    }

    fn parse_identifier(&mut self) -> Expression {
        Expression::Identifier(Identifier::new(self.cur_token.clone()))
    }

    fn cur_token_is(&self, t: &Token) -> bool {
        std::mem::discriminant(&self.cur_token) == std::mem::discriminant(t)
    }

    fn peek_token_is(&self, t: &Token) -> bool {
        std::mem::discriminant(&self.peek_token) == std::mem::discriminant(t)
    }

    fn expect_peek(&mut self, t: &Token) -> bool {
        if self.peek_token_is(t) {
            self.next_token();
            return true;
        }

        self.peek_err(t);
        false
    }

    fn peek_err(&mut self, t: &Token) {
        let msg = format!(
            "expected next token to be {t}, got {} instead",
            self.peek_token
        );
        self.errs.push(msg);
    }

    fn register_prefix(&mut self, t: Token, func: PrefixParseFn) {
        self.prefix_parse_fns.insert(t, func);
    }

    fn register_infix(&mut self, t: Token, func: InfixParseFn) {
        self.infix_parse_fns.insert(t, func);
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::ast::base::Node;

    #[test]
    fn test_let_statements() {
        let input = "
            let x = 5;
            let y = 10;
            let foobar = 838383;
        ";
        let l = Lexer::new(input.to_string());
        let mut p = Parser::new(l);
        let program = p.parse_program();
        check_parser_errs(&p);

        assert_eq!(
            program.statements.len(),
            3,
            "program.statements does not contain 3 statements. got={}",
            program.statements.len()
        );

        let tests = vec!["x", "y", "foobar"];

        for (i, expected_identifier) in tests.iter().enumerate() {
            let stmt = &program.statements[i];
            if !test_let_statement(stmt, expected_identifier) {
                return;
            }
        }
    }

    fn test_let_statement(s: &Statement, name: &str) -> bool {
        if s.token_literal() != "let" {
            eprintln!("s.token_literal not 'let'. got={}", s.token_literal());
            return false;
        }

        let let_stmt = match s {
            Statement::Let(stmt) => stmt,
            _ => {
                println!("s not etStatement");
                return false;
            }
        };

        if let_stmt.name.value() != name {
            println!(
                "let_stmt.name.val not '{}'. got={}",
                name,
                let_stmt.name.value()
            );
            return false;
        }

        if let_stmt.name.token_literal() != name {
            println!(
                "s.name not '{}'. got={}",
                name,
                let_stmt.name.token_literal()
            );
            return false;
        }

        true
    }

    #[test]
    fn test_return_statements() {
        let input = "
                return 5;
                return 10;
                return 993322;
        ";
        let l = Lexer::new(input.to_string());
        let mut p = Parser::new(l);
        let program = p.parse_program();
        check_parser_errs(&p);

        assert_eq!(
            program.statements.len(),
            3,
            "program.statements does not contain 3 statements. got={}",
            program.statements.len()
        );

        for stmt in program.statements {
            match stmt {
                Statement::Return(s) => {
                    if s.token_literal() != "return" {
                        eprintln!("s.token_literal not 'return'. got={}", s.token_literal());
                    }
                }
                _ => eprintln!("stmt is not ReturnStatement. Got: {}", stmt.token_literal()),
            }
        }
    }

    #[test]
    fn test_identifier_expression() {
        let input = "foobar";
        let l = Lexer::new(input.to_string());
        let mut p = Parser::new(l);
        let program = p.parse_program();
        check_parser_errs(&p);

        assert_eq!(
            program.statements.len(),
            1,
            "program.statements does not contain 1 statements. got={}",
            program.statements.len()
        );

        let stmt = &program.statements[0];
        match stmt {
            Statement::Expression(exp_stmt) => {
                assert_eq!(exp_stmt.expression.to_string(), "foobar");
                assert_eq!(exp_stmt.token_literal(), "foobar");
            }
            _ => eprintln!(
                "program.Statements[0] is not ast.ExpressionStatement. got={}",
                stmt.token_literal()
            ),
        }
    }

    fn check_parser_errs(p: &Parser) {
        let errs = p.errs();

        if errs.is_empty() {
            return;
        }

        println!("parser has {} errors", errs.len());
        for msg in errs {
            eprintln!("parser err: {msg}");
        }

        panic!("Fail now");
    }
}
