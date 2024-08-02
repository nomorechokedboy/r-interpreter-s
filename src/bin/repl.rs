use anyhow::Result;
use r_interpreter_s::{lexer::Lexer, token::Token};

fn main() -> Result<()> {
    std::io::stdin().lines().for_each(|line| {
        if let Ok(line) = line {
            let mut tokenizer = Lexer::new(line);

            while tokenizer.next_token() != Token::Eof {
                let token = tokenizer.next_token();
                println!("{} ", token);
            }
        }
    });
    return Ok(());
}
