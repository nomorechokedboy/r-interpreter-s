#[cfg(test)]
mod test {
    use crate::{
        ast::base::{Expression, Node, Statement},
        lexer::Lexer,
        parser::Parser,
    };
    use std::{boxed, ops::Deref};

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
        Bool(bool),
    }

    fn test_literal_expression(exp: &Expression, expected: Expected) -> bool {
        match expected {
            Expected::Int(val) => {
                test_integer_literal(exp, val.try_into().expect("shouldn't get error here"))
            }
            Expected::Int64(val) => test_integer_literal(exp, val),
            Expected::String(val) => test_identifier(exp, val),
            Expected::Bool(b) => test_bool_literal(exp, b),
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

    fn test_bool_literal(expr: &Expression, val: bool) -> bool {
        match expr {
            Expression::Bool(b) => {
                assert_eq!(b.value(), val, "b.value() not {val}. got={}", b.value());
                assert_eq!(
                    b.token_literal(),
                    val.to_string(),
                    "b.token_literal() not {val}. got={}",
                    b.token_literal()
                );

                true
            }
            _ => {
                eprintln!("expr not Expression::Bool. got={expr:#?}");
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
        int_val: Expected,
    }

    #[test]
    fn test_parsing_prefix_expressions() {
        let prefix_tests = vec![
            PrefixTest {
                input: "!5".to_string(),
                operator: "!".to_string(),
                int_val: Expected::Int64(5),
            },
            PrefixTest {
                input: "-15".to_string(),
                operator: "-".to_string(),
                int_val: Expected::Int64(15),
            },
            PrefixTest {
                input: "!true".to_string(),
                operator: "!".to_string(),
                int_val: Expected::Bool(true),
            },
            PrefixTest {
                input: "!false".to_string(),
                operator: "!".to_string(),
                int_val: Expected::Bool(false),
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
                        test_literal_expression(&pre_exp.right.deref().clone(), t.int_val);
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

    #[derive(Debug)]
    struct InfixTest {
        input: String,
        left_val: Expected,
        operator: String,
        right_val: Expected,
    }

    #[test]
    fn test_parsing_infix_expressions() {
        let infix_tests = vec![
            InfixTest {
                input: "5 + 5".to_string(),
                left_val: Expected::Int64(5),
                operator: "+".to_string(),
                right_val: Expected::Int64(5),
            },
            InfixTest {
                input: "5 - 5".to_string(),
                left_val: Expected::Int64(5),
                operator: "-".to_string(),
                right_val: Expected::Int64(5),
            },
            InfixTest {
                input: "5 * 5".to_string(),
                left_val: Expected::Int64(5),
                operator: "*".to_string(),
                right_val: Expected::Int64(5),
            },
            InfixTest {
                input: "5 / 5".to_string(),
                left_val: Expected::Int64(5),
                operator: "/".to_string(),
                right_val: Expected::Int64(5),
            },
            InfixTest {
                input: "5 > 5".to_string(),
                left_val: Expected::Int64(5),
                operator: ">".to_string(),
                right_val: Expected::Int64(5),
            },
            InfixTest {
                input: "5 < 5".to_string(),
                left_val: Expected::Int64(5),
                operator: "<".to_string(),
                right_val: Expected::Int64(5),
            },
            InfixTest {
                input: "5 == 5".to_string(),
                left_val: Expected::Int64(5),
                operator: "==".to_string(),
                right_val: Expected::Int64(5),
            },
            InfixTest {
                input: "5 != 5".to_string(),
                left_val: Expected::Int64(5),
                operator: "!=".to_string(),
                right_val: Expected::Int64(5),
            },
            InfixTest {
                input: "true == true".to_string(),
                left_val: Expected::Bool(true),
                operator: "==".to_string(),
                right_val: Expected::Bool(true),
            },
            InfixTest {
                input: "true != false".to_string(),
                left_val: Expected::Bool(true),
                operator: "!=".to_string(),
                right_val: Expected::Bool(false),
            },
            InfixTest {
                input: "false == false".to_string(),
                left_val: Expected::Bool(false),
                operator: "==".to_string(),
                right_val: Expected::Bool(false),
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
                        test_literal_expression(&in_exp.left.deref().clone(), t.left_val);
                        assert_eq!(in_exp.operator, t.operator);
                        test_literal_expression(&in_exp.right.deref().clone(), t.right_val);
                    }
                    _ => panic!("exp not IntegerLiteral. got={:?}", exp_stmt.expression),
                },
                _ => panic!("program.statements[0] is not an ExpressionStatement. got={stmt:#?}",),
            }
        }
    }

    #[derive(Debug)]
    struct TestOperatorPrecedenceParsingInput {
        input: String,
        expected: String,
    }

    #[test]
    fn test_operator_precedence_parsing() {
        let input = vec![
            TestOperatorPrecedenceParsingInput {
                input: "-a * b".to_string(),
                expected: "((-a) * b)".to_string(),
            },
            TestOperatorPrecedenceParsingInput {
                input: "!-a".to_string(),
                expected: "(!(-a))".to_string(),
            },
            TestOperatorPrecedenceParsingInput {
                input: "a + b + c".to_string(),
                expected: "((a + b) + c)".to_string(),
            },
            TestOperatorPrecedenceParsingInput {
                input: "a + b - c".to_string(),
                expected: "((a + b) - c)".to_string(),
            },
            TestOperatorPrecedenceParsingInput {
                input: "a * b * c".to_string(),
                expected: "((a * b) * c)".to_string(),
            },
            TestOperatorPrecedenceParsingInput {
                input: "a * b / c".to_string(),
                expected: "((a * b) / c)".to_string(),
            },
            TestOperatorPrecedenceParsingInput {
                input: "a + b / c".to_string(),
                expected: "(a + (b / c))".to_string(),
            },
            TestOperatorPrecedenceParsingInput {
                input: "a + b * c + d / e - f".to_string(),
                expected: "(((a + (b * c)) + (d / e)) - f)".to_string(),
            },
            TestOperatorPrecedenceParsingInput {
                input: "3 + 4; -5 * 5".to_string(),
                expected: "(3 + 4)((-5) * 5)".to_string(),
            },
            TestOperatorPrecedenceParsingInput {
                input: "5 > 4 == 3 < 4".to_string(),
                expected: "((5 > 4) == (3 < 4))".to_string(),
            },
            TestOperatorPrecedenceParsingInput {
                input: "5 < 4 != 3 > 4".to_string(),
                expected: "((5 < 4) != (3 > 4))".to_string(),
            },
            TestOperatorPrecedenceParsingInput {
                input: "3 + 4 * 5 == 3 * 1 + 4 * 5".to_string(),
                expected: "((3 + (4 * 5)) == ((3 * 1) + (4 * 5)))".to_string(),
            },
            TestOperatorPrecedenceParsingInput {
                input: "true".to_string(),
                expected: "true".to_string(),
            },
            TestOperatorPrecedenceParsingInput {
                input: "false".to_string(),
                expected: "false".to_string(),
            },
            TestOperatorPrecedenceParsingInput {
                input: "3 > 5 == false".to_string(),
                expected: "((3 > 5) == false)".to_string(),
            },
            TestOperatorPrecedenceParsingInput {
                input: "3 < 5 == true".to_string(),
                expected: "((3 < 5) == true)".to_string(),
            },
            TestOperatorPrecedenceParsingInput {
                input: "1 + (2 + 3) + 4".to_string(),
                expected: "((1 + (2 + 3)) + 4)".to_string(),
            },
            TestOperatorPrecedenceParsingInput {
                input: "(5 + 5) * 2".to_string(),
                expected: "((5 + 5) * 2)".to_string(),
            },
            TestOperatorPrecedenceParsingInput {
                input: "2 / (5 + 5)".to_string(),
                expected: "(2 / (5 + 5))".to_string(),
            },
            TestOperatorPrecedenceParsingInput {
                input: "-(5 + 5)".to_string(),
                expected: "(-(5 + 5))".to_string(),
            },
            TestOperatorPrecedenceParsingInput {
                input: "!(true == true)".to_string(),
                expected: "(!(true == true))".to_string(),
            },
        ];
        for t in input {
            let l = Lexer::new(t.input);
            let mut p = Parser::new(l);
            let program = p.parse_program();
            check_parser_errs(&p);

            assert_eq!(
                t.expected,
                program.to_string(),
                "expected={}, got={}",
                t.expected,
                program.to_string()
            );
        }
    }

    #[test]
    fn test_if_expr() {
        let input = "if (x < y) { x }".to_string();
        let l = Lexer::new(input);
        let mut p = Parser::new(l);
        let program = p.parse_program();
        check_parser_errs(&p);

        assert_eq!(
            program.statements.len(),
            1,
            "program.statements doesn't contain 1 statements. got={}",
            program.statements.len()
        );

        let stmt = &program.statements[0];
        match stmt {
            Statement::Expression(expr) => match &expr.expression {
                Expression::IfExpression(if_expr) => {
                    if !test_infix_expression(
                        *if_expr.condition.clone(),
                        Expected::String("x".to_string()),
                        "<".to_string(),
                        Expected::String("y".to_string()),
                    ) {
                        panic!("lmao")
                    }

                    assert_eq!(
                        if_expr.consequence.statements.len(),
                        1,
                        "consequence is not 1 statements. got={:#?}",
                        if_expr.consequence.statements.len()
                    );
                    match &if_expr.consequence.statements[0] {
                        Statement::Expression(expr) => {
                            if !test_identifier(&expr.expression, "x".to_string()) {
                                panic!("lmao")
                            }
                        }
                        _ => panic!(
                            "statements[0] isn't Expression. got={:#?}",
                            if_expr.consequence.statements[0]
                        ),
                    };

                    assert_eq!(
                        if_expr.alternative.is_none(),
                        true,
                        "if_expr.alternative was not nil. got={:#?}",
                        if_expr.alternative
                    )
                }
                _ => panic!(
                    "expr.expression is not IfExpression. got={:#?}",
                    expr.expression,
                ),
            },
            _ => panic!("program.statements[0] is not Statement::Expression. got: {stmt:#?}"),
        }
    }

    #[test]
    fn test_if_else_expr() {
        let input = "if (x < y) { x } else { y }";
        let l = Lexer::new(input.to_string());
        let mut p = Parser::new(l);
        let program = p.parse_program();
        check_parser_errs(&p);

        assert_eq!(
            program.statements.len(),
            1,
            "program.statements doesn't contain 1 statements. got={}",
            program.statements.len()
        );

        let stmt = &program.statements[0];
        match stmt {
            Statement::Expression(expr) => match &expr.expression {
                Expression::IfExpression(if_expr) => {
                    if !test_infix_expression(
                        *if_expr.condition.clone(),
                        Expected::String("x".to_string()),
                        "<".to_string(),
                        Expected::String("y".to_string()),
                    ) {
                        panic!("lmao")
                    }

                    assert_eq!(
                        if_expr.consequence.statements.len(),
                        1,
                        "consequence is not 1 statements. got={:#?}",
                        if_expr.consequence.statements.len()
                    );
                    match &if_expr.consequence.statements[0] {
                        Statement::Expression(expr) => {
                            if !test_identifier(&expr.expression, "x".to_string()) {
                                panic!("lmao")
                            }
                        }
                        _ => panic!(
                            "statements[0] isn't Expression. got={:#?}",
                            if_expr.consequence.statements[0]
                        ),
                    };

                    assert_eq!(
                        if_expr.alternative.is_some(),
                        true,
                        "if_expr.alternative was not nil. got={:#?}",
                        if_expr.alternative
                    );
                    if let Some(alt) = &if_expr.alternative {
                        assert_eq!(
                            alt.statements.len(),
                            1,
                            "alternative is not 1 statements. got={:#?}",
                            alt.statements.len()
                        );
                        match &alt.statements[0] {
                            Statement::Expression(expr) => {
                                if !test_identifier(&expr.expression, "y".to_string()) {
                                    panic!("lmao")
                                }
                            }
                            _ => panic!(
                                "statements[0] isn't Expression. got={:#?}",
                                alt.statements[0]
                            ),
                        };
                    }
                }
                _ => panic!(
                    "expr.expression is not IfExpression. got={:#?}",
                    expr.expression,
                ),
            },
            _ => panic!("program.statements[0] is not Statement::Expression. got: {stmt:#?}"),
        }
    }

    #[test]
    fn test_function_literal_parsing() {
        let input = "fn(x, y) { x + y; }";
        let l = Lexer::new(input.to_string());
        let mut p = Parser::new(l);
        let program = p.parse_program();
        check_parser_errs(&p);

        assert_eq!(
            program.statements.len(),
            1,
            "program.statements doesn't contain 1 statements. got={}",
            program.statements.len()
        );

        let stmt = &program.statements[0];
        match stmt {
            Statement::Expression(expr) => match &expr.expression {
                Expression::FunctionLiteral(expr) => {
                    assert_eq!(
                        2,
                        expr.params.len(),
                        "function literal params are wrong. want 2, got={}",
                        expr.params.len()
                    );
                    test_literal_expression(&expr.params[0], Expected::String("x".to_string()));
                    test_literal_expression(&expr.params[1], Expected::String("y".to_string()));
                    let body = expr.body.clone().expect("Should have a body");
                    assert_eq!(1, body.statements.len());
                    let body_stmt = &body.statements[0];
                    match body_stmt {
                        Statement::Expression(stmt) => test_infix_expression(
                            stmt.expression.clone(),
                            Expected::String("x".to_string()),
                            "+".to_string(),
                            Expected::String("y".to_string()),
                        ),
                        _ => panic!("function body stmt is not Expression. got: {body_stmt:#?}"),
                    };
                }
                _ => panic!(
                    "expr.expression is not FunctionLiteral. got={:#?}",
                    expr.expression,
                ),
            },
            _ => panic!("program.statements[0] is not Statement::Expression. got: {stmt:#?}"),
        }
    }

    #[test]
    fn test_function_params_parsing() {
        let tests = vec![
            ("fn() {};", Vec::<&str>::new()),
            ("fn(x) {};", vec!["x"]),
            ("fn(x, y, z) {};", vec!["x", "y", "z"]),
        ];
        for (input, expected_params) in tests {
            let l = Lexer::new(input.to_string());
            let mut p = Parser::new(l);
            let program = p.parse_program();
            check_parser_errs(&p);

            assert_eq!(
                program.statements.len(),
                1,
                "program.statements doesn't contain 1 statements. got={}",
                program.statements.len()
            );

            let stmt = &program.statements[0];
            match stmt {
                Statement::Expression(expr) => match &expr.expression {
                    Expression::FunctionLiteral(expr) => {
                        assert_eq!(
                            expected_params.len(),
                            expr.params.len(),
                            "function literal params are wrong. want 2, got={}",
                            expr.params.len()
                        );

                        for (i, &ident) in expected_params.iter().enumerate() {
                            test_literal_expression(
                                &expr.params[i],
                                Expected::String(ident.to_string()),
                            );
                        }
                    }
                    _ => panic!(
                        "expr.expression is not FunctionLiteral. got={:#?}",
                        expr.expression,
                    ),
                },
                _ => panic!("program.statements[0] is not Statement::Expression. got: {stmt:#?}"),
            }
        }
    }

    #[test]
    fn test_call_expression_parsing() {
        let input = "add(1, 2 * 3, 4 + 5);";
        let l = Lexer::new(input.to_string());
        let mut p = Parser::new(l);
        let program = p.parse_program();
        check_parser_errs(&p);

        assert_eq!(
            program.statements.len(),
            1,
            "program.statements doesn't contain 1 statements. got={}",
            program.statements.len()
        );

        let stmt = &program.statements[0];
        match stmt {
            Statement::Expression(expr) => match &expr.expression {
                Expression::CallExpression(expr) => {
                    let _ = !test_identifier(&*expr.function, "add".to_string());
                    assert_eq!(
                        3,
                        expr.args.len(),
                        "wrong len of args. got: {}",
                        expr.args.len()
                    );

                    test_literal_expression(&expr.args[0], Expected::Int64(1));
                    test_infix_expression(
                        expr.args[1].clone(),
                        Expected::Int64(2),
                        "*".to_string(),
                        Expected::Int64(3),
                    );
                    test_infix_expression(
                        expr.args[2].clone(),
                        Expected::Int64(4),
                        "+".to_string(),
                        Expected::Int64(5),
                    );
                }
                _ => panic!("expr is not CallExpression. got={:#?}", expr.expression),
            },
            _ => panic!("program.statements[0] is not Statement::Expression. got: {stmt:#?}"),
        }
    }
}
