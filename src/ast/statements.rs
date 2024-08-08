use super::base::{Expression, Node, Statement};
use crate::token::Token;
use std::{
    fmt::{self, Display},
    io::Write,
};

#[derive(Clone, Debug)]
pub struct LetStatement {
    pub token: Token,
    pub name: Identifier,
    pub val: Option<Expression>,
}

impl LetStatement {
    pub fn new(token: Token, name: Identifier, val: Option<Expression>) -> Self {
        return Self { token, name, val };
    }
}

impl Node for LetStatement {
    fn token_literal(&self) -> String {
        self.token.to_string()
    }
}

impl Display for LetStatement {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let val = match &self.val {
            Some(t) => format!("{t}"),
            None => "".to_string(),
        };
        let let_stmt = format!(
            "{} {} = {};",
            self.token_literal(),
            self.name.to_string(),
            val
        );

        write!(f, "{}", let_stmt)
    }
}

#[derive(Clone, Debug)]
pub struct ReturnStatement {
    pub token: Token,
    pub val: Option<Expression>,
}

impl ReturnStatement {
    pub fn new(token: Token, val: Option<Expression>) -> Self {
        return Self { token, val };
    }
}

impl Node for ReturnStatement {
    fn token_literal(&self) -> String {
        self.token.to_string()
    }
}

impl Display for ReturnStatement {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let val = match &self.val {
            Some(t) => format!("{t}"),
            None => "".to_string(),
        };
        let return_stmt = format!("{} {};", self.token_literal(), val);

        write!(f, "{}", return_stmt)
    }
}

#[derive(Clone, Debug)]
pub struct ExpressionStatement {
    pub token: Token,
    pub expression: Expression,
}

impl Node for ExpressionStatement {
    fn token_literal(&self) -> String {
        self.token.to_string()
    }
}

impl Display for ExpressionStatement {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.expression.to_string())
    }
}

#[derive(Clone, Debug)]
pub struct Identifier {
    pub token: Token,
}

impl Identifier {
    pub fn new(token: Token) -> Self {
        match token {
            Token::Ident(_) => Self { token },
            _ => panic!(
                "Identifier must be created with Token::Ident, got {:?}",
                token
            ),
        }
    }

    pub fn value(&self) -> &str {
        match &self.token {
            Token::Ident(s) => s,
            _ => panic!("Expected Token::Ident, got {:#?}", self.token),
        }
    }
}

impl Display for Identifier {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.value())
    }
}

impl Node for Identifier {
    fn token_literal(&self) -> String {
        self.token.to_string()
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct IntegerLiteral {
    pub token: Token,
    pub val: i64,
}

impl IntegerLiteral {
    pub fn new(token: Token, val: i64) -> Self {
        Self { token, val }
    }
}

impl Node for IntegerLiteral {
    fn token_literal(&self) -> String {
        self.token.to_string()
    }
}

impl Display for IntegerLiteral {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.val)
    }
}

#[derive(Clone, Debug)]
pub struct PrefixExpression {
    pub token: Token,
    pub operator: String,
    pub right: Box<Expression>,
}

impl PrefixExpression {
    pub fn new(token: Token, operator: String, right: Box<Expression>) -> Self {
        Self {
            token,
            operator,
            right,
        }
    }
}

impl Node for PrefixExpression {
    fn token_literal(&self) -> String {
        self.token.to_string()
    }
}

impl Display for PrefixExpression {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "({}{})", self.operator, self.right.to_string())
    }
}

#[derive(Clone, Debug)]
pub struct InfixExpression {
    pub token: Token,
    pub operator: String,
    pub right: Box<Expression>,
    pub left: Box<Expression>,
}

impl InfixExpression {
    pub fn new(
        token: Token,
        operator: String,
        right: Box<Expression>,
        left: Box<Expression>,
    ) -> Self {
        Self {
            token,
            operator,
            right,
            left,
        }
    }
}

impl Node for InfixExpression {
    fn token_literal(&self) -> String {
        self.token.to_string()
    }
}

impl Display for InfixExpression {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "({} {} {})",
            self.left.to_string(),
            self.operator,
            self.right.to_string()
        )
    }
}

#[derive(Clone, Debug)]
pub struct Bool {
    pub token: Token,
}

impl Bool {
    pub fn new(token: Token) -> Self {
        Self { token }
    }

    pub fn value(&self) -> bool {
        match &self.token {
            Token::True => true,
            Token::False => false,
            _ => panic!("Expected Token::True|False, got: {:#?}", self.token),
        }
    }
}

impl Node for Bool {
    fn token_literal(&self) -> String {
        self.token.to_string()
    }
}

impl Display for Bool {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.token_literal(),)
    }
}

#[derive(Clone, Debug)]
pub struct BlockStatement {
    pub token: Token,
    pub statements: Vec<Statement>,
}

impl BlockStatement {
    pub fn new(token: Token, statements: Vec<Statement>) -> Self {
        Self { token, statements }
    }
}

impl Display for BlockStatement {
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

impl Node for BlockStatement {
    fn token_literal(&self) -> String {
        self.token.to_string()
    }
}

#[derive(Clone, Debug)]
pub struct If {
    pub token: Token,
    pub condition: Box<Expression>,
    pub consequence: BlockStatement,
    pub alternative: Option<BlockStatement>,
}

impl If {
    pub fn new(
        token: Token,
        condition: Box<Expression>,
        consequence: BlockStatement,
        alternative: Option<BlockStatement>,
    ) -> Self {
        Self {
            token,
            condition,
            consequence,
            alternative,
        }
    }
}

impl Display for If {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let alternative = if let Some(alt) = &self.alternative {
            format!("else{}", alt.to_string())
        } else {
            String::new()
        };
        write!(
            f,
            "if{} {}{}",
            self.condition.to_string(),
            self.consequence.to_string(),
            alternative
        )
    }
}

impl Node for If {
    fn token_literal(&self) -> String {
        self.token.to_string()
    }
}

#[derive(Clone, Debug)]
pub struct Function {
    pub token: Token,
    pub params: Vec<Expression>,
    pub body: Option<BlockStatement>,
}

impl Function {
    pub fn new(token: Token, params: Vec<Expression>, body: Option<BlockStatement>) -> Self {
        Self {
            token,
            params,
            body,
        }
    }
}

impl Node for Function {
    fn token_literal(&self) -> String {
        self.token.to_string()
    }
}

impl Display for Function {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut params = vec![];
        for p in &self.params {
            params.push(p.to_string())
        }
        let body = match &self.body {
            None => String::new(),
            Some(block_stmt) => block_stmt.to_string(),
        };

        write!(
            f,
            "{}({}) {}",
            self.token_literal(),
            params.join(", "),
            body
        )
    }
}

#[derive(Clone, Debug)]
pub struct Call {
    pub args: Vec<Expression>,
    pub function: Box<Expression>,
    pub token: Token,
}

impl Node for Call {
    fn token_literal(&self) -> String {
        self.token.to_string()
    }
}

impl Display for Call {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut args = vec![];
        for arg in &self.args {
            args.push(arg.to_string());
        }

        write!(f, "{}({})", self.function.to_string(), args.join(", "))
    }
}
