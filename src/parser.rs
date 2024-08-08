use crate::{
    ast::{
        base::{Expression, Statement},
        program::Program,
        statements::{
            BlockStatement, Bool, ExpressionStatement, Function, Identifier, If, InfixExpression,
            IntegerLiteral, LetStatement, PrefixExpression, ReturnStatement,
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
        parser.register_prefix(Token::True, Parser::parse_bool);
        parser.register_prefix(Token::False, Parser::parse_bool);
        parser.register_prefix(Token::Lparen, Parser::parse_group_expression);
        parser.register_prefix(Token::If, Parser::parse_if_expression);
        parser.register_prefix(Token::Function, Parser::parse_function_literal);

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

    fn parse_bool(&mut self) -> Option<Expression> {
        match &self.cur_token {
            Token::True | Token::False => Some(Expression::Bool(Bool::new(self.cur_token.clone()))),
            _ => None,
        }
    }

    fn parse_group_expression(&mut self) -> Option<Expression> {
        self.next_token();
        let expr = self.parse_expression(Precedence::Lowest);

        if !self.expect_peek(&Token::Rparen) {
            return None;
        }

        return expr;
    }

    fn parse_if_expression(&mut self) -> Option<Expression> {
        let token = self.cur_token.clone();
        if !self.expect_peek(&Token::Lparen) {
            return None;
        }

        self.next_token();
        let condition = self.parse_expression(Precedence::Lowest);
        if !self.expect_peek(&Token::Rparen) {
            return None;
        }

        if !self.expect_peek(&Token::Lbrace) {
            return None;
        }

        let mut expr = If::new(
            token,
            Box::new(condition?),
            self.parse_block_statement(),
            None,
        );
        if self.peek_token_is(&Token::Else) {
            self.next_token();
            if !self.expect_peek(&Token::Lbrace) {
                return None;
            }

            expr.alternative = Some(self.parse_block_statement());
        }

        Some(Expression::IfExpression(expr))
    }

    fn parse_block_statement(&mut self) -> BlockStatement {
        let mut block = BlockStatement::new(self.cur_token.clone(), vec![]);
        self.next_token();
        while !self.cur_token_is(&Token::Rbrace) && !self.cur_token_is(&Token::Eof) {
            let stmt = self.parse_statement();
            if let Some(stmt) = stmt {
                block.statements.push(stmt);
            }

            self.next_token();
        }

        block
    }

    fn parse_function_literal(&mut self) -> Option<Expression> {
        let token = self.cur_token.clone();
        if !self.expect_peek(&Token::Lparen) {
            return None;
        }

        let params = self.parse_function_params()?;
        if !self.expect_peek(&Token::Lbrace) {
            return None;
        }

        let body = self.parse_block_statement();
        let lit = Function::new(token, params, Some(body));
        Some(Expression::FunctionLiteral(lit))
    }

    fn parse_function_params(&mut self) -> Option<Vec<Expression>> {
        let mut identifiers = Vec::<Expression>::new();
        if self.peek_token_is(&Token::Rparen) {
            self.next_token();
            return Some(identifiers);
        }

        self.next_token();
        let ident = Expression::Identifier(Identifier::new(self.cur_token.clone()));
        identifiers.push(ident);
        while self.peek_token_is(&Token::Comma) {
            self.next_token();
            self.next_token();
            let ident = Expression::Identifier(Identifier::new(self.cur_token.clone()));
            identifiers.push(ident);
        }
        if !self.expect_peek(&Token::Rparen) {
            return None;
        }

        Some(identifiers)
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
