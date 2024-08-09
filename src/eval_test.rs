#[cfg(test)]
mod test {
    use crate::{ast::base::ASTNode, eval::eval, lexer::Lexer, object::Object, parser::Parser};

    #[test]
    fn test_eval_int_expr() {
        let tests = vec![("5", 5), ("10", 10)];
        for (input, expected) in tests {
            let evaluated = test_eval(input);
            test_int_obj(evaluated.expect("should not nil"), expected);
        }
    }

    fn test_eval(input: &str) -> Option<Object> {
        let l = Lexer::new(input.to_string());
        let mut p = Parser::new(l);
        let program = p.parse_program();

        eval(ASTNode::Program(program))
    }

    fn test_int_obj(obj: Object, expected: i64) -> bool {
        match obj {
            Object::Int64(int) => {
                assert_eq!(
                    int, expected,
                    "object has wrong value. got={int}, want={expected}"
                );

                true
            }
            _ => {
                eprintln!("object has wrong value. got={obj:#?}, want {expected}");
                false
            }
        }
    }
}
