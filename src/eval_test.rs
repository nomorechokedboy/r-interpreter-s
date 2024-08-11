#[cfg(test)]
mod test {
    use crate::{ast::base::ASTNode, eval::eval, lexer::Lexer, object::Object, parser::Parser};

    #[test]
    fn test_eval_int_expr() {
        let tests = vec![
            ("5", 5),
            ("10", 10),
            ("-5", -5),
            ("-10", -10),
            ("5 + 5 + 5 + 5 - 10", 10),
            ("2 * 2 * 2 * 2 * 2", 32),
            ("-50 + 100 - 50", 0),
            ("5 * 2 + 10", 20),
            ("5 + 2 * 10", 25),
            ("20 + 2 * -10", 0),
            ("50 / 2 * 2 + 10", 60),
            ("2 * (5 + 10)", 30),
            ("3 * 3 * 3 + 10", 37),
            ("3 * (3 * 3) + 10", 37),
            ("(5 + 10 * 2 + 15 / 3) * 2 + -10", 50),
        ];
        for (input, expected) in tests {
            let evaluated = test_eval(input);
            test_int_obj(evaluated.expect("should not nil"), expected);
        }
    }

    #[test]
    fn test_eval_bool_expr() {
        let tests = vec![
            ("true", true),
            ("false", false),
            ("1 < 2", true),
            ("1 > 2", false),
            ("1 < 1", false),
            ("1 > 1", false),
            ("1 == 1", true),
            ("1 != 1", false),
            ("1 == 2", false),
            ("1 != 2", true),
            ("(1 < 2) == true", true),
            ("(1 < 2) == false", false),
            ("(1 > 2) == true", false),
            ("(1 > 2) == false", true),
            ("(5 > 5 == true) != false", false),
        ];
        for (input, expected) in tests {
            let evaluated = test_eval(input);
            test_bool_obj(evaluated.expect("should not be None"), expected);
        }
    }

    #[test]
    fn test_bang_operator() {
        let tests = vec![
            ("!true", false),
            ("!false", true),
            ("!5", false),
            ("!!true", true),
            ("!!false", false),
            ("!!5", true),
        ];
        for (input, expected) in tests {
            let evaluated = test_eval(input);
            test_bool_obj(evaluated.expect("should not be None"), expected);
        }
    }

    #[test]
    fn test_if_else_epxr() {
        let tests = vec![
            ("if (true) { 10 }", Some(10)),
            ("if (false) { 10 }", None),
            ("if (1) { 10 }", Some(10)),
            ("if (1 < 2) { 10 }", Some(10)),
            ("if (1 > 2) { 10 }", None),
            ("if (1 < 2) { 10 } else { 20 }", Some(10)),
            ("if (1 > 2) { 10 } else { 20 }", Some(20)),
        ];
        for (input, expected) in tests {
            let evaluated = test_eval(input);
            match expected {
                Some(val) => test_int_obj(evaluated.expect("should not None"), val),
                None => test_null_obj(evaluated.expect("should not None")),
            };
        }
    }

    #[test]
    fn test_return_statements() {
        let tests = vec![
            ("return 10;", 10),
            ("return 10; 9;", 10),
            ("return 2 * 5; 9;", 10),
            ("9; return 2 * 5; 9;", 10),
            (
                "if (10 > 1) {
                    if (10 > 1) {
                        return 10;
                    }

                    return 1;
                }",
                10,
            ),
        ];
        for (input, expected) in tests {
            let evaluated = test_eval(input);
            test_int_obj(evaluated.expect("shound not None"), expected);
        }
    }

    #[test]
    fn test_error_handling() {
        let tests = vec![
            ("5 + true;", "type mismatch: INTEGER + BOOLEAN"),
            ("5 + true; 5;", "type mismatch: INTEGER + BOOLEAN"),
            ("-true", "unknown operator: -BOOLEAN"),
            ("true + false", "unknown operator: BOOLEAN + BOOLEAN"),
            ("5; true + false; 5", "unknown operator: BOOLEAN + BOOLEAN"),
            (
                "if (10 > 1) { true + false; }",
                "unknown operator: BOOLEAN + BOOLEAN",
            ),
            (
                "
             if (10 > 1) {
                 if (10 > 1) {
                     return true + false;
                 }

                 return 1;
             }
             ",
                "unknown operator: BOOLEAN + BOOLEAN",
            ),
        ];
        for (input, expected) in tests {
            let evaluated = test_eval(input);
            if let Some(Object::Error(msg)) = &evaluated {
                assert_eq!(
                    msg, expected,
                    "wrong error message. expected={expected}, got={msg}"
                );
            } else {
                panic!("no error object returned. got={evaluated:#?}");
            }
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

    fn test_null_obj(obj: Object) -> bool {
        match obj {
            Object::Null => true,
            _ => {
                eprintln!("object is not NULL. got={obj:#?}");
                false
            }
        }
    }
}
