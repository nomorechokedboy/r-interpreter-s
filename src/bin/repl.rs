use anyhow::Result;
use r_interpreter_s::{ast::base::ASTNode, eval::eval, lexer::Lexer, parser::Parser, token::Token};
use std::{
    env,
    io::{self, BufReader, Write},
};

const PROMPT: &str = ">> ";
const MONKEY_FACE: &str = r##"            __,__
   .--.  .-"     "-.  .--.
  / .. \/  .-. .-.  \/ .. \
 | |  '|  /   Y   \  |'  | |
 | \   \  \ 0 | 0 /  /   / |
  \ '- ,\.-"""""""-./, -' /
   ''-' /_   ^ ^   _\ '-''
       |  \._   _./  |
       \   \ '~' /   /
        '._ '-=-' _.'
           '-----'
"##;

fn main() -> Result<()> {
    let username = env::var("USER").unwrap_or_else(|_| "User".to_string());

    println!(
        "Hello {}! This is the Monkey programming language!",
        username
    );
    println!("Feel free to type in commands");
    start(BufReader::new(io::stdin()), io::stdout())
}

fn print_parser_errs(out: &mut dyn Write, errs: &[String]) -> Result<()> {
    writeln!(out, "{}", MONKEY_FACE)?;
    writeln!(out, "Woops! We ran into some monkey business here!")?;
    writeln!(out, " parser errors:")?;
    for msg in errs {
        writeln!(out, "\t{}", msg)?;
    }
    Ok(())
}

fn start(mut input: impl io::BufRead, mut output: impl Write) -> Result<()> {
    loop {
        write!(output, "{}", PROMPT)?;
        output.flush()?;

        let mut line = String::new();
        if input.read_line(&mut line)? == 0 {
            return Ok(());
        }

        let l = Lexer::new(line);
        let mut p = Parser::new(l);
        let program = p.parse_program();

        if !p.errs().is_empty() {
            print_parser_errs(&mut output, &p.errs())?;
            continue;
        }

        if let Some(obj) = eval(ASTNode::Program(program)) {
            writeln!(output, "{}", obj.inspect())?
        }
    }
}
