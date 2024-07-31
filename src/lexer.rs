use anyhow::{bail, Result};

use crate::token::Token;

#[derive(Debug)]
pub struct Lexer {
    input: Vec<u8>,
    position: usize,
    read_position: usize,
    ch: u8,
}

impl Lexer {
    pub fn new(input: String) -> Self {
        let mut l = Self {
            position: 0,
            read_position: 0,
            ch: 0,
            input: input.into_bytes(),
        };
        l.read_char();

        l
    }

    pub fn next_token(&mut self) -> Result<Token> {
        self.skip_whitespace();

        let token = match self.ch {
            b'{' => Token::Lbrace,
            b'}' => Token::Rbrace,
            b'(' => Token::Lparen,
            b')' => Token::Rparen,

            b',' => Token::Comma,
            b';' => Token::Semicolon,

            b'+' => Token::Plus,
            /* b'-' => Token::Dash,
            b'!' => {
                if self.peek() == b'=' {
                    self.read_char();
                    Token::NotEqual
                } else {
                    Token::Bang
                }
            },
            b'>' => Token::GreaterThan,
            b'<' => Token::LessThan,
            b'*' => Token::Asterisk,
            b'/' => Token::ForwardSlash, */
            b'=' => {
                /* if self.peek() == b'=' {
                    self.read_char();
                    Token::Equal
                } else { */
                    Token::Assign
                // }
            },
            b'a'..=b'z' | b'A'..=b'Z' | b'_' => {
                let ident = self.read_ident();
                return Ok(match ident.as_str() {
                    "fn" => Token::Function,
                    "let" => Token::Let,
                    /* "if" => Token::If,
                    "false" => Token::False,
                    "true" => Token::True,
                    "return" => Token::Return,
                    "else" => Token::Else, */
                    _ => Token::Ident(ident),
                });
            },
            b'0'..=b'9' => return Ok(Token::Int(self.read_int())),
            0 => Token::Eof,
            _ => unreachable!("no monkey program should contain these characters and you should feel bad about yourself")
        };

        self.read_char();
        Ok(token)
    }

    fn read_char(&mut self) {
        if self.read_position >= self.input.len() {
            self.ch = 0;
        } else {
            self.ch = self.input[self.read_position];
        }

        self.position = self.read_position;
        self.read_position += 1;
    }

    fn is_letter(&self, ch: u8) -> bool {
        ch.is_ascii_alphabetic() || ch == b'_'
    }

    fn read_ident(&mut self) -> String {
        let pos = self.position;
        while self.is_letter(self.ch) {
            self.read_char();
        }

        String::from_utf8_lossy(&self.input[pos..self.position]).to_string()
    }

    fn skip_whitespace(&mut self) {
        while self.ch.is_ascii_whitespace() {
            self.read_char();
        }
    }

    fn read_int(&mut self) -> String {
        let pos = self.position;
        while self.ch.is_ascii_digit() {
            self.read_char();
        }

        String::from_utf8_lossy(&self.input[pos..self.position]).to_string()
    }
}

#[cfg(test)]
mod test {
    use anyhow::Result;

    use super::{Lexer, Token};

    #[test]
    fn get_next_token() -> Result<()> {
        let input = "=+(){},;";
        let mut lexer = Lexer::new(input.into());

        let tokens = vec![
            Token::Assign,
            Token::Plus,
            Token::Lparen,
            Token::Rparen,
            Token::Lbrace,
            Token::Rbrace,
            Token::Comma,
            Token::Semicolon,
        ];

        for token in tokens {
            let next_token = lexer.next_token()?;
            println!("expected: {:?}, received {:?}", token, next_token);
            assert_eq!(token, next_token);
        }

        return Ok(());
    }
}
