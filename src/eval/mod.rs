use crate::{ast::ast, object};

pub fn eval(program: ast::Program, env: &mut object::Environment) -> object::Object {
    let mut obj = object::Object::Null;

    for stmt in program.statements {
        match eval_statement(stmt, env) {
            object::Object::Return(inner_obj) => {
                return *inner_obj;
            }

            other => obj = other,
        }
    }

    obj
}

fn eval_block_statement(
    block: ast::BlockStatement,
    env: &mut object::Environment,
) -> object::Object {
    let mut obj = object::Object::Null;

    for stmt in block.statements {
        obj = eval_statement(stmt, env);
        if obj.is_type(object::ObjectType::Return) {
            return obj;
        }
    }

    obj
}

fn eval_statement(stmt: ast::Statement, env: &mut object::Environment) -> object::Object {
    match stmt {
        ast::Statement::Let(let_stmt) => eval_let_statment(let_stmt, env),
        ast::Statement::Return(ret_stmt) => eval_return_statement(ret_stmt, env),
        ast::Statement::Expression(exp_stmt) => eval_expression_statement(exp_stmt, env),
    }
}

fn eval_let_statment(stmt: ast::LetStatement, env: &mut object::Environment) -> object::Object {
    let obj = eval_expression(stmt.value, env);
    env.set(&stmt.name, &obj);

    object::Object::Null
}

fn eval_return_statement(
    stmt: ast::ReturnStatement,
    env: &mut object::Environment,
) -> object::Object {
    object::Object::Return(Box::new(eval_expression(stmt.expression, env)))
}

fn eval_expression_statement(
    stmt: ast::ExpressionStatement,
    env: &mut object::Environment,
) -> object::Object {
    eval_expression(stmt.expression, env)
}

fn eval_expression(exp: ast::Expression, env: &mut object::Environment) -> object::Object {
    match exp {
        ast::Expression::Empty() => object::Object::Null,
        ast::Expression::Identifier(ident) => eval_identifier(ident, env),
        ast::Expression::Integer(il) => eval_integer_literal(il),
        ast::Expression::Bool(boolean) => eval_boolean(boolean),
        ast::Expression::Prefix(prefix) => eval_prefix_expression(prefix, env),
        ast::Expression::Infix(infix) => eval_infix_expression(infix, env),
        ast::Expression::If(if_exp) => eval_if_expression(if_exp, env),
        ast::Expression::Fucntion(function) => eval_function_literal(function, env),
        ast::Expression::Call(call) => eval_call_expression(call, env),
    }
}

fn eval_identifier(ident: ast::Identifier, env: &object::Environment) -> object::Object {
    let obj = env.get(&ident).unwrap();
    obj
}

fn eval_integer_literal(il: ast::IntegerLiteral) -> object::Object {
    object::Object::Integer(il.value)
}

fn eval_boolean(b: ast::Boolean) -> object::Object {
    object::Object::Boolean(b.value)
}

fn eval_prefix_expression(
    prefix: ast::PrefixExpression,
    env: &mut object::Environment,
) -> object::Object {
    match prefix.operator.as_str() {
        "!" => {
            let obj = eval_expression(*prefix.right, env);
            eval_bang_operator_expression(obj)
        }
        "-" => {
            let obj = eval_expression(*prefix.right, env);
            eval_minus_operator_expression(obj)
        }
        op => panic!("unsupported perfix operator {}", op),
    }
}

fn eval_infix_expression(
    infix: ast::InfixExpression,
    env: &mut object::Environment,
) -> object::Object {
    let left_exp = *infix.left;
    let right_exp = *infix.right;
    let operator = infix.operator;

    let lobj = eval_expression(left_exp, env);
    let robj = eval_expression(right_exp, env);

    assert_eq!(lobj.type_of(), robj.type_of());

    match (lobj, robj) {
        (object::Object::Integer(l), object::Object::Integer(r)) => match operator.as_str() {
            "+" => object::Object::Integer(l + r),
            "-" => object::Object::Integer(l - r),
            "*" => object::Object::Integer(l * r),
            "/" => object::Object::Integer(l / r),
            "==" => object::Object::Boolean(l == r),
            "!=" => object::Object::Boolean(l != r),
            ">" => object::Object::Boolean(l > r),
            "<" => object::Object::Boolean(l < r),
            op => panic!("Integer object does not support infix operator '{}'", op),
        },

        (object::Object::Boolean(l), object::Object::Boolean(r)) => match operator.as_str() {
            "==" => object::Object::Boolean(l == r),
            "!=" => object::Object::Boolean(l != r),
            op => panic!("Boolean object does not support infix operator '{}'", op),
        },

        _ => panic!("only Integer object can be infix operator!"),
    }
}

fn eval_if_expression(if_exp: ast::IfExpression, env: &mut object::Environment) -> object::Object {
    let cond_obj = eval_expression(*if_exp.condition, env);

    let cond = match cond_obj {
        object::Object::Null => false,
        object::Object::Boolean(b) => b,
        object::Object::Integer(n) => n != 0,
        _ => false, // how to handle it?
    };

    if cond {
        eval_block_statement(if_exp.consequence, env)
    } else {
        if let Some(alternative) = if_exp.alternative {
            eval_block_statement(alternative, env)
        } else {
            object::Object::Null
        }
    }
}

fn eval_function_literal(
    function: ast::FunctionLiteral,
    env: &mut object::Environment,
) -> object::Object {
    object::Object::Function(function.parameters, function.body, env.clone())
}

fn eval_call_expression(
    call: ast::CallExpression,
    env: &mut object::Environment,
) -> object::Object {
    let func_obj = eval_expression(*call.function, env);
    let mut arg_objs = vec![];
    for arg in call.arguments {
        arg_objs.push(eval_expression(arg, env));
    }

    apply_function(func_obj, arg_objs)
}

fn apply_function(function: object::Object, args: Vec<object::Object>) -> object::Object {
    match function {
        object::Object::Function(identifiers, body, env) => {
            let mut new_env = object::Environment::new(Some(Box::new(env.clone())));
            assert_eq!(args.len(), identifiers.len());

            for i in 0..args.len() {
                new_env.set(&identifiers[i], &args[i]);
            }

            let obj = eval_block_statement(body, &mut new_env);

            match obj {
                object::Object::Return(ret_obj) => *ret_obj,
                o => o,
            }
        }
        _ => panic!("expecte Function object!"),
    }
}

fn eval_bang_operator_expression(obj: object::Object) -> object::Object {
    match obj {
        object::Object::Boolean(b) => object::Object::Boolean(!b),
        object::Object::Integer(i) => object::Object::Boolean(i == 0),
        object::Object::Null => object::Object::Boolean(true),
        _ => object::Object::Boolean(false),
    }
}

fn eval_minus_operator_expression(obj: object::Object) -> object::Object {
    if let object::Object::Integer(n) = obj {
        object::Object::Integer(-n)
    } else {
        panic!("minus prefix operator only support Integer object!")
    }
}

#[cfg(test)]
mod tests {
    use crate::{lexer, parser};

    use super::*;

    #[test]
    fn test_eval_integer_literal() {
        let tests = vec![
            ("0", 0),
            ("10", 10),
            ("-1", -1),
            ("-10", -10),
            ("--10", 10),
            ("1 + 2", 3),
            ("1 - 1", 0),
            ("1 * 2", 2),
            ("10 / 2", 5),
        ];

        for (input, expected) in tests {
            let obj = test_eval_helper(input);
            test_integer_object(obj, expected);
        }
    }

    #[test]
    fn test_eval_boolean() {
        let tests = vec![
            ("true", true),
            ("false", false),
            ("1 == 1", true),
            ("1 != 1", false),
            ("1 == 0", false),
            ("1 != 0", true),
            ("1 > 2", false),
            ("1 < 2", true),
            ("2 > 1", true),
            ("2 < 1", false),
            ("true == true", true),
            ("false == false", true),
            ("true == false", false),
            ("true != false", true),
            ("(1 == 1) == true", true),
            ("(1 == 1) == false", false),
            ("(1 != 1) == false", true),
            ("(1 != 1) != false", false),
            ("(1 > 2) == false", true),
            ("(1 < 2) == true", true),
            ("(1 > 2) != false", false),
            ("(1 < 2) != true", false),
        ];

        for (input, expected) in tests {
            let obj = test_eval_helper(input);
            test_boolean_object(obj, expected);
        }
    }

    #[test]
    fn test_bang_operator() {
        let tests = vec![
            ("!true", false),
            ("!false", true),
            ("!!true", true),
            ("!!false", false),
            ("!0", true),
            ("!!0", false),
            ("!10", false),
        ];

        for (input, expected) in tests {
            let obj = test_eval_helper(input);
            test_boolean_object(obj, expected);
        }
    }

    #[test]
    fn test_if_expression() {
        let tests = vec![
            ("if (true) { 10 }", object::Object::Integer(10)),
            ("if (false) { 10 }", object::Object::Null),
            ("if (1) { 10 }", object::Object::Integer(10)),
            ("if (0) { 10 }", object::Object::Null),
            ("if (1 < 2) { 10 }", object::Object::Integer(10)),
            ("if (1 > 2) { 10 }", object::Object::Null),
            ("if (1 < 2) { 10 } else { 20 }", object::Object::Integer(10)),
            ("if (1 > 2) { 10 } else { 20 }", object::Object::Integer(20)),
        ];

        for (input, expected) in tests {
            assert_eq!(test_eval_helper(input), expected);
        }
    }

    #[test]
    fn test_return_statement() {
        let tests = vec![
            ("return 10;", object::Object::Integer(10)),
            ("return true;", object::Object::Boolean(true)),
            ("return 2 * 5; 0;", object::Object::Integer(10)),
            ("1; return 2 * 5; 0", object::Object::Integer(10)),
            (
                "if (1 > 0) { if (1 > 0) { return 10; } return 1;}",
                object::Object::Integer(10),
            ),
        ];

        for (input, expected) in tests {
            assert_eq!(test_eval_helper(input), expected);
        }
    }

    #[test]
    fn test_let_statement() {
        let tests = vec![
            ("let a = 5; a;", object::Object::Integer(5)),
            ("let a = 5 * 5; a;", object::Object::Integer(25)),
            ("let a = 5; let b = a; b;", object::Object::Integer(5)),
            (
                "let a = 5; let b = a; let c = a + b + 5; c;",
                object::Object::Integer(15),
            ),
        ];

        for (input, expected) in tests {
            assert_eq!(test_eval_helper(input), expected);
        }
    }

    #[test]
    fn test_function_literal() {
        let input = "fn(a, b) { a + b; }";

        let obj = test_eval_helper(input);

        match obj {
            object::Object::Function(params, block, _) => {
                // check params
                assert_eq!(params.len(), 2);
                assert_eq!(params[0].value, "a");
                assert_eq!(params[1].value, "b");

                // check body has one expression statement: (a + b)
                assert_eq!(block.statements.len(), 1);
                match &block.statements[0] {
                    ast::Statement::Expression(exp_stmt) => match &exp_stmt.expression {
                        ast::Expression::Infix(infix) => {
                            // operator
                            assert_eq!(infix.operator, "+");
                            // left identifier == a
                            if let ast::Expression::Identifier(id) = &*infix.left {
                                assert_eq!(id.value, "a");
                            } else {
                                panic!("left of infix is not Identifier 'a'");
                            }
                            // right identifier == b
                            if let ast::Expression::Identifier(id) = &*infix.right {
                                assert_eq!(id.value, "b");
                            } else {
                                panic!("right of infix is not Identifier 'b'");
                            }
                        }
                        other => panic!("expected Infix expression, got {:?}", other),
                    },
                    other => panic!("expected ExpressionStatement, got {:?}", other),
                }
            }
            other => panic!("expected Function object, got {:?}", other),
        }
    }

    #[test]
    fn test_function_call() {
        let tests = vec![
            ("let foo = fn(x) {x;} foo(1)", object::Object::Integer(1)),
            (
                "let add = fn(x, y) {x + y;} add(1, 2)",
                object::Object::Integer(3),
            ),
        ];

        for (input, expected) in tests {
            assert_eq!(test_eval_helper(input), expected);
        }
    }

    fn test_eval_helper(input: &str) -> object::Object {
        let lexer = lexer::Lexer::new(input);
        let mut parser = parser::Parser::new(lexer);
        let program = parser.parse_program().unwrap();

        let mut env = object::Environment::new(None);

        eval(program, &mut env)
    }

    fn test_integer_object(obj: object::Object, expected: i64) {
        if let object::Object::Integer(n) = obj {
            assert_eq!(n, expected);
        } else {
            panic!("expect Integer object!");
        }
    }

    fn test_boolean_object(obj: object::Object, expected: bool) {
        if let object::Object::Boolean(n) = obj {
            assert_eq!(n, expected);
        } else {
            panic!("expect Boolean object!");
        }
    }
}
