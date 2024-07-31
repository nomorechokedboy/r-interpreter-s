use std::fmt::Display;

#[derive(Debug, PartialEq)]
pub enum Token {
    Ident(String),
    Int(String),

    Illegal,
    Eof,

    Assign,
    Plus,
    Minus,
    Bang,
    Asterisk,
    Slash,

    LessThan,
    GreaterThan,

    Comma,
    Semicolon,

    Lparen,
    Rparen,
    Lbrace,
    Rbrace,

    Function,
    Let,

    If,
    Else,
    Return,
    True,
    False,
}

impl Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        return match self {
            Token::Ident(x) => write!(f, "Ident({})", x),
            Token::Int(x) => write!(f, "Int({})", x),
            Token::Illegal => write!(f, "Illegal"),
            Token::Eof => write!(f, "Eof"),
            Token::Assign => write!(f, "Assign"),
            Token::Bang => write!(f, "Bang"),
            Token::Asterisk => write!(f, "Asterisk"),
            Token::Minus => write!(f, "Minus"),
            Token::Slash => write!(f, "Slash"),
            /* Token::Equal => write!(f, "Equal"),
            Token::NotEqual => write!(f, "NotEqual"), */
            Token::LessThan => write!(f, "LessThan"),
            Token::GreaterThan => write!(f, "GreaterThan"),
            Token::Plus => write!(f, "Plus"),
            Token::Comma => write!(f, "Comma"),
            Token::Semicolon => write!(f, "Semicolon"),
            Token::Lparen => write!(f, "Lparen"),
            Token::Rparen => write!(f, "Rparen"),
            Token::Lbrace => write!(f, "Lbrace"),
            Token::Rbrace => write!(f, "Rbrace"),
            Token::Function => write!(f, "Function"),
            Token::Let => write!(f, "Let"),
            Token::If => write!(f, "If"),
            Token::Else => write!(f, "Else"),
            Token::Return => write!(f, "Return"),
            Token::True => write!(f, "True"),
            Token::False => write!(f, "False"),
        }
    }
}
