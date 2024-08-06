use crate::{
    ast::{
        base::{Expression, Statement},
        program::Program,
        statements::{
            ExpressionStatement, Identifier, InfixExpression, IntegerLiteral, LetStatement,
            PrefixExpression, ReturnStatement,
        },
    },
    lexer::Lexer,
    token::Token,
};
use lazy_static::lazy_static;
use std::collections::HashMap;

lazy_static! {
    static ref PRECEDENCES: HashMap<Token, Precedence> = {
        let m = HashMap::from_iter(vec![
            (Token::Equal, Precedence::Equals),
            (Token::NotEqual, Precedence::Equals),
            (Token::LessThan, Precedence::LessGreater),
            (Token::GreaterThan, Precedence::LessGreater),
            (Token::Plus, Precedence::Sum),
            (Token::Minus, Precedence::Sum),
            (Token::Slash, Precedence::Product),
            (Token::Asterisk, Precedence::Product),
        ]);

        m
    };
}

type PrefixParseFn = for<'a> fn(&'a mut Parser) -> Option<Expression>;
type InfixParseFn = for<'a> fn(&'a mut Parser, Expression) -> Option<Expression>;

pub struct Parser {
    cur_token: Token,
    errs: Vec<String>,
    lexer: Lexer,
    peek_token: Token,
    prefix_parse_fns: HashMap<Token, PrefixParseFn>,
    infix_parse_fns: HashMap<Token, InfixParseFn>,
}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
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
        parser.register_prefix(Token::Int(String::new()), Parser::parse_integer_literal);
        parser.register_prefix(Token::Bang, Parser::parse_prefix_expression);
        parser.register_prefix(Token::Minus, Parser::parse_prefix_expression);

        parser.register_infix(Token::Plus, Parser::parse_infix_expression);
        parser.register_infix(Token::Minus, Parser::parse_infix_expression);
        parser.register_infix(Token::Slash, Parser::parse_infix_expression);
        parser.register_infix(Token::Asterisk, Parser::parse_infix_expression);
        parser.register_infix(Token::Equal, Parser::parse_infix_expression);
        parser.register_infix(Token::NotEqual, Parser::parse_infix_expression);
        parser.register_infix(Token::LessThan, Parser::parse_infix_expression);
        parser.register_infix(Token::GreaterThan, Parser::parse_infix_expression);

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
        let token = self.cur_token.clone();
        let prefix_parse = self.prefix_parse_fns.iter().find_map(|(key, &value)| {
            if std::mem::discriminant(key) == std::mem::discriminant(&token) {
                Some(value)
            } else {
                None
            }
        });

        let mut left_exp = match prefix_parse {
            Some(parse_fn) => parse_fn(self),
            None => {
                self.no_prefix_parse_fn_err(&token);
                None
            }
        };

        while !self.peek_token_is(&Token::Semicolon) && &precedence < &self.peek_precedence() {
            let maybe_infix_parse = self.infix_parse_fns.get(&self.peek_token);

            match maybe_infix_parse {
                None => {
                    return left_exp;
                }
                Some(&infix) => {
                    self.next_token();
                    left_exp = infix(self, left_exp?);
                }
            };
        }

        left_exp
    }

    fn parse_identifier(&mut self) -> Option<Expression> {
        Some(Expression::Identifier(Identifier::new(
            self.cur_token.clone(),
        )))
    }

    fn parse_integer_literal(&mut self) -> Option<Expression> {
        match &self.cur_token {
            Token::Int(s) => {
                let value = s.parse::<i64>().ok()?;
                Some(Expression::IntegerLiteral(IntegerLiteral::new(
                    self.cur_token.clone(),
                    value,
                )))
            }
            _ => None,
        }
    }

    fn parse_prefix_expression(&mut self) -> Option<Expression> {
        let (token, operator) = (self.cur_token.clone(), self.cur_token.to_string());
        self.next_token();

        Some(Expression::PrefixExpression(PrefixExpression::new(
            token,
            operator,
            Box::new(self.parse_expression(Precedence::Prefix)?),
        )))
    }

    fn parse_infix_expression(&mut self, left: Expression) -> Option<Expression> {
        let (token, operator, precedence, left) = (
            self.cur_token.clone(),
            self.cur_token.to_string(),
            self.cur_precedence(),
            Box::new(left),
        );
        self.next_token();

        let right = Box::new(self.parse_expression(precedence)?);
        Some(Expression::InfixExpression(InfixExpression::new(
            token, operator, right, left,
        )))
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

    fn no_prefix_parse_fn_err(&mut self, t: &Token) {
        self.errs
            .push(format!("no prefix parse function for {} found", t))
    }

    fn peek_precedence(&self) -> Precedence {
        match PRECEDENCES.get(&self.peek_token) {
            Some(p) => p.clone(),
            None => Precedence::Lowest,
        }
    }

    fn cur_precedence(&self) -> Precedence {
        match PRECEDENCES.get(&self.cur_token) {
            Some(p) => p.clone(),
            None => Precedence::Lowest,
        }
    }
}

#[cfg(test)]
mod test {
    use std::ops::Deref;

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
            eprintln!("s.token_literal not 'let'. got={s:#?}");
            return false;
        }

        let let_stmt = match s {
            Statement::Let(stmt) => stmt,
            _ => {
                println!("s not LetStatement");
                return false;
            }
        };

        if let_stmt.name.value() != name {
            println!(
                "let_stmt.name.val not '{name}'. got={}",
                let_stmt.name.value()
            );
            return false;
        }

        if let_stmt.name.token_literal() != name {
            println!("s.name not '{name}'. got={}", let_stmt.name.token_literal());
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
                        eprintln!("s.token_literal not 'return'. got={s:#?}");
                    }
                }
                _ => panic!("stmt is not ReturnStatement. Got: {stmt:#?}"),
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
            _ => eprintln!("program.Statements[0] is not ast.ExpressionStatement. got={stmt:#?}",),
        }
    }

    #[test]
    fn test_iteger_literal_expression() {
        let input = "5";
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
            Statement::Expression(exp_stmt) => match &exp_stmt.expression {
                Expression::IntegerLiteral(int_lit) => {
                    assert_eq!(int_lit.val, 5);
                    assert_eq!(int_lit.token_literal(), "5");
                }
                _ => panic!("exp not IntegerLiteral. got={:?}", exp_stmt.expression),
            },
            _ => panic!("program.statements[0] is not an ExpressionStatement. got={stmt:#?}",),
        }
    }

    #[derive(Debug)]
    struct PrefixTest {
        input: String,
        operator: String,
        int_val: i64,
    }

    #[test]
    fn test_parsing_prefix_expressions() {
        let prefix_tests = vec![
            PrefixTest {
                input: "!5".to_string(),
                operator: "!".to_string(),
                int_val: 5,
            },
            PrefixTest {
                input: "-15".to_string(),
                operator: "-".to_string(),
                int_val: 15,
            },
        ];

        for t in prefix_tests {
            let l = Lexer::new(t.input);
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
                Statement::Expression(exp_stmt) => match &exp_stmt.expression {
                    Expression::PrefixExpression(pre_exp) => {
                        assert_eq!(pre_exp.operator, t.operator);
                        test_integer_literal(&pre_exp.right.deref().clone(), t.int_val);
                    }
                    _ => panic!("exp not IntegerLiteral. got={:?}", exp_stmt.expression),
                },
                _ => panic!(
                    "program.statements[0] is not an ExpressionStatement. got={:#?}",
                    stmt
                ),
            }
        }
    }

    fn test_integer_literal(maybe_int_lit: &Expression, val: i64) -> bool {
        match maybe_int_lit {
            Expression::IntegerLiteral(int_lit) => {
                assert_eq!(
                    int_lit.val, val,
                    "int_lit.val {} is not equal to {val}",
                    int_lit.val
                );
                assert_eq!(int_lit.token_literal(), val.to_string());
                true
            }
            _ => {
                eprintln!(
                    "test_integer_literal err: expect IntegerLiteral, got: {maybe_int_lit:#?}"
                );
                false
            }
        }
    }

    #[derive(Debug)]
    struct InfixTest {
        input: String,
        left_val: i64,
        operator: String,
        right_val: i64,
    }

    #[test]
    fn test_parsing_infix_expressions() {
        let infix_tests = vec![
            InfixTest {
                input: "5 + 5".to_string(),
                left_val: 5,
                operator: "+".to_string(),
                right_val: 5,
            },
            InfixTest {
                input: "5 - 5".to_string(),
                left_val: 5,
                operator: "-".to_string(),
                right_val: 5,
            },
            InfixTest {
                input: "5 * 5".to_string(),
                left_val: 5,
                operator: "*".to_string(),
                right_val: 5,
            },
            InfixTest {
                input: "5 / 5".to_string(),
                left_val: 5,
                operator: "/".to_string(),
                right_val: 5,
            },
            InfixTest {
                input: "5 > 5".to_string(),
                left_val: 5,
                operator: ">".to_string(),
                right_val: 5,
            },
            InfixTest {
                input: "5 < 5".to_string(),
                left_val: 5,
                operator: "<".to_string(),
                right_val: 5,
            },
            InfixTest {
                input: "5 == 5".to_string(),
                left_val: 5,
                operator: "==".to_string(),
                right_val: 5,
            },
            InfixTest {
                input: "5 != 5".to_string(),
                left_val: 5,
                operator: "!=".to_string(),
                right_val: 5,
            },
        ];
        for t in infix_tests {
            let l = Lexer::new(t.input);
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
                Statement::Expression(exp_stmt) => match &exp_stmt.expression {
                    Expression::InfixExpression(in_exp) => {
                        test_integer_literal(&in_exp.left.deref().clone(), t.left_val);
                        assert_eq!(in_exp.operator, t.operator);
                        test_integer_literal(&in_exp.right.deref().clone(), t.right_val);
                    }
                    _ => panic!("exp not IntegerLiteral. got={:?}", exp_stmt.expression),
                },
                _ => panic!("program.statements[0] is not an ExpressionStatement. got={stmt:#?}",),
            }
        }
    }

    fn test_identifier(exp: &Expression, val: String) -> bool {
        match exp {
            Expression::Identifier(ident) => {
                if ident.value().to_string() != val {
                    eprintln!("ident.value not {val}. got: {}", ident.value());
                    return false;
                }

                if ident.token_literal() != val {
                    eprintln!(
                        "ident.token_literal not {val}. got: {}",
                        ident.token_literal()
                    );
                    return false;
                }

                true
            }
            _ => {
                eprintln!("exp not Expression::Identifier. Got: {exp:#?}");
                false
            }
        }
    }

    #[derive(Debug)]
    enum Expected {
        Int(isize),
        Int64(i64),
        String(String),
    }

    fn test_literal_expression(exp: &Expression, expected: Expected) -> bool {
        match expected {
            Expected::Int(val) => {
                test_integer_literal(exp, val.try_into().expect("shouldn't get error here"))
            }
            Expected::Int64(val) => test_integer_literal(exp, val),
            Expected::String(val) => test_identifier(exp, val),
        }
    }

    fn test_infix_expression(
        expr: Expression,
        left: Expected,
        operator: String,
        right: Expected,
    ) -> bool {
        match expr {
            Expression::InfixExpression(in_expr) => {
                if !test_literal_expression(in_expr.left.deref(), left) {
                    return false;
                }

                if in_expr.operator != operator {
                    eprintln!("expr.operator is not {operator}. got={}", in_expr.operator);
                    return false;
                }

                if !test_literal_expression(in_expr.right.deref(), right) {
                    return false;
                }

                true
            }
            _ => {
                eprintln!("expr is not InfixExpression. got={expr:#?}");
                false
            }
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
