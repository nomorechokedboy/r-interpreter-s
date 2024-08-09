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

    #[test]
    fn test_eval_bool_expr() {
        let tests = vec![("true", true), ("false", false)];
        for (input, expected) in tests {
            let evaluated = test_eval(input);
            test_bool_obj(evaluated.expect("should not be None"), expected);
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
            Object::Int64(val) => {
                assert_eq!(
                    val, expected,
                    "object has wrong value. got={val}, want={expected}"
                );

                true
            }
            _ => {
                eprintln!("object has wrong value. got={obj:#?}, want {expected}");
                false
            }
        }
    }

    fn test_bool_obj(obj: Object, expected: bool) -> bool {
        match obj {
            Object::Bool(val) => {
                assert_eq!(
                    val, expected,
                    "object has wrong value. got={val}, want={expected}"
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
