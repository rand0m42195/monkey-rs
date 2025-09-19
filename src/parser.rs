use crate::{ast, errors::MonkeyError, lexer, token};
use std::collections::HashMap;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum Precedence {
    Lowest = 0,
    Equals,      // ==
    LessGreater, // < >
    Sum,         // + -
    Product,     // * /
    Prefix,      // -X !X
    Call,        // fn(x)
    Index,       // arr[1]
}

impl Precedence {
    fn from_token(typ: token::TokenType) -> Self {
        match typ {
            token::TokenType::EQ | token::TokenType::NotEq => Precedence::Equals,
            token::TokenType::LT | token::TokenType::GT => Precedence::LessGreater,
            token::TokenType::PLUS | token::TokenType::MINUS => Precedence::Sum,
            token::TokenType::ASTERISK | token::TokenType::SLASH => Precedence::Product,
            token::TokenType::LPAREN => Precedence::Call,
            token::TokenType::LBRACKET => Precedence::Index,
            _ => Precedence::Lowest,
        }
    }
}

pub struct Parser {
    lexer: lexer::Lexer,
    cur_token: token::Token,
    peek_token: token::Token,

    prefix_parse_fns:
        HashMap<token::TokenType, fn(&mut Parser) -> Result<ast::Expression, MonkeyError>>,
    infix_parse_fns: HashMap<
        token::TokenType,
        fn(&mut Parser, ast::Expression) -> Result<ast::Expression, MonkeyError>,
    >,
}

impl Parser {
    pub fn new(lexer: lexer::Lexer) -> Parser {
        let mut parser = Parser {
            lexer,
            cur_token: token::Token::new(token::TokenType::ILLEGAL, "".to_string()),
            peek_token: token::Token::new(token::TokenType::ILLEGAL, "".to_string()),
            prefix_parse_fns: HashMap::new(),
            infix_parse_fns: HashMap::new(),
        };

        parser.register_prefix_fn(token::TokenType::IDENT, Self::parse_identifier);
        parser.register_prefix_fn(token::TokenType::INT, Self::parse_integer);
        parser.register_prefix_fn(token::TokenType::STRING, Self::parse_string_literal);
        parser.register_prefix_fn(token::TokenType::MINUS, Self::parse_prefix_expression);
        parser.register_prefix_fn(token::TokenType::BANG, Self::parse_prefix_expression);
        parser.register_prefix_fn(token::TokenType::TRUE, Self::parse_boolean);
        parser.register_prefix_fn(token::TokenType::FALSE, Self::parse_boolean);
        parser.register_prefix_fn(token::TokenType::LPAREN, Self::parse_grouped_expression);
        parser.register_prefix_fn(token::TokenType::IF, Self::parse_if_expression);
        parser.register_prefix_fn(token::TokenType::FUNCTION, Self::parse_function_literal);
        parser.register_prefix_fn(token::TokenType::LBRACKET, Self::parse_array);

        parser.register_infix_fn(token::TokenType::PLUS, Self::parse_infix_expression);
        parser.register_infix_fn(token::TokenType::MINUS, Self::parse_infix_expression);
        parser.register_infix_fn(token::TokenType::SLASH, Self::parse_infix_expression);
        parser.register_infix_fn(token::TokenType::ASTERISK, Self::parse_infix_expression);
        parser.register_infix_fn(token::TokenType::EQ, Self::parse_infix_expression);
        parser.register_infix_fn(token::TokenType::NotEq, Self::parse_infix_expression);
        parser.register_infix_fn(token::TokenType::LT, Self::parse_infix_expression);
        parser.register_infix_fn(token::TokenType::GT, Self::parse_infix_expression);
        parser.register_infix_fn(token::TokenType::LPAREN, Self::parse_call_expression);
        parser.register_infix_fn(token::TokenType::LBRACKET, Self::parse_index_expression);

        parser.next_token();
        parser.next_token();

        parser
    }

    pub fn parse_program(&mut self) -> Result<ast::Program, MonkeyError> {
        let mut program = ast::Program { statements: vec![] };

        while self.cur_token.typ() != token::TokenType::EOF {
            let stmt = self.parse_statement()?;
            program.statements.push(stmt);
            self.next_token();
        }

        Ok(program)
    }

    fn parse_statement(&mut self) -> Result<ast::Statement, MonkeyError> {
        match self.cur_token.typ() {
            token::TokenType::LET => Ok(ast::Statement::Let(self.parse_let_statement()?)),
            token::TokenType::RETURN => Ok(ast::Statement::Return(self.parse_return_statement()?)),
            _ => Ok(ast::Statement::Expression(ast::ExpressionStatement {
                expression: self.parse_expression_statement()?,
            })),
        }
    }

    fn parse_let_statement(&mut self) -> Result<ast::LetStatement, MonkeyError> {
        self.expect_peek(token::TokenType::IDENT)?;

        let ident = ast::Identifier {
            value: self.cur_token.literal().clone(),
        };

        self.expect_peek(token::TokenType::ASSIGN)?;

        // now self.cur_token is '=', skip it.
        self.next_token();

        let exp = self.parse_expression(Precedence::Lowest)?;

        while self.peek_token_is(token::TokenType::SEMICOLON) {
            self.next_token();
        }

        Ok(ast::LetStatement {
            name: ident,
            value: exp,
        })
    }

    fn parse_return_statement(&mut self) -> Result<ast::ReturnStatement, MonkeyError> {
        self.next_token();

        let exp = self.parse_expression(Precedence::Lowest)?;

        if self.peek_token_is(token::TokenType::SEMICOLON) {
            self.next_token();
        }

        Ok(ast::ReturnStatement { expression: exp })
    }

    fn parse_expression_statement(&mut self) -> Result<ast::Expression, MonkeyError> {
        let exp = self.parse_expression(Precedence::Lowest)?;
        while self.peek_token_is(token::TokenType::SEMICOLON) {
            self.next_token();
        }
        Ok(exp)
    }

    fn parse_expression(&mut self, precedence: Precedence) -> Result<ast::Expression, MonkeyError> {
        if let Some(prefix_fn) = self.prefix_parse_fns.get(&self.cur_token.typ()) {
            let mut left_exp = prefix_fn(self)?;

            while !self.cur_token_is(token::TokenType::SEMICOLON)
                && precedence < self.peek_token_precedence()
            {
                if let Some(infix_fn) = self.infix_parse_fns.get(&self.peek_token.typ()) {
                    left_exp = infix_fn(self, left_exp)?;
                } else {
                    return Ok(left_exp);
                }
            }

            Ok(left_exp)
        } else {
            Err(MonkeyError::SyntaxError {
                message: format!(
                    "no prefix parse function for {} found",
                    self.cur_token.typ()
                ),
            })
        }
    }

    fn parse_identifier(&mut self) -> Result<ast::Expression, MonkeyError> {
        if self.cur_token_is(token::TokenType::IDENT) {
            Ok(ast::Expression::Ident(ast::Identifier {
                value: self.cur_token.literal().clone(),
            }))
        } else {
            Err(MonkeyError::TypeError {
                expected: token::TokenType::IDENT.to_string(),
                actual: self.cur_token.typ().to_string(),
            })
        }
    }

    fn parse_integer(&mut self) -> Result<ast::Expression, MonkeyError> {
        if !self.cur_token_is(token::TokenType::INT) {
            return Err(MonkeyError::TypeError {
                expected: token::TokenType::INT.to_string(),
                actual: self.cur_token.typ().to_string(),
            });
        }

        let n = self.cur_token.literal().parse::<i64>();
        if n.is_err() {
            return Err(MonkeyError::SyntaxError {
                message: format!(
                    "can not parse {} to integer: {:?}",
                    self.cur_token.literal(),
                    n
                ),
            });
        }

        Ok(ast::Expression::Int(ast::Integer::new(n.unwrap())))
    }

    fn parse_string_literal(&mut self) -> Result<ast::Expression, MonkeyError> {
        if !self.cur_token_is(token::TokenType::STRING) {
            return Err(MonkeyError::TypeError {
                expected: token::TokenType::STRING.to_string(),
                actual: self.cur_token.typ().to_string(),
            });
        }

        Ok(ast::Expression::String(ast::MString::new(
            &self.cur_token.literal(),
        )))
    }

    fn parse_array(&mut self) -> Result<ast::Expression, MonkeyError> {
        let exps = self.parse_expressions(token::TokenType::RBRACKET)?;
        Ok(ast::Expression::Array(ast::ArrayLiteral { elems: exps }))
    }

    fn parse_boolean(&mut self) -> Result<ast::Expression, MonkeyError> {
        let exp = if self.cur_token_is(token::TokenType::TRUE) {
            ast::Boolean { value: true }
        } else {
            ast::Boolean { value: false }
        };
        Ok(ast::Expression::Bool(exp))
    }

    fn parse_prefix_expression(&mut self) -> Result<ast::Expression, MonkeyError> {
        let operator = self.cur_token.literal().clone();
        // consume the prefix operator, after call next_token
        // self.cur_token is the start of the right expression
        self.next_token();

        if let Some(prefix_fn) = self.prefix_parse_fns.get(&self.cur_token.typ()) {
            let exp = prefix_fn(self)?;
            Ok(ast::Expression::Prefix(ast::PrefixExpression {
                operator,
                right: Box::new(exp),
            }))
        } else {
            Err(MonkeyError::SyntaxError {
                message: format!(
                    "no prefix parse function for {} found",
                    self.cur_token.typ()
                ),
            })
        }
    }

    fn parse_infix_expression(
        &mut self,
        left: ast::Expression,
    ) -> Result<ast::Expression, MonkeyError> {
        self.next_token();
        let operator = self.cur_token.literal().clone();
        let precedence = Precedence::from_token(self.cur_token.typ());
        // consume the infix operator, after call next_token
        // self.cur_token is the start of the right expression
        self.next_token();

        let right = self.parse_expression(precedence)?;
        Ok(ast::Expression::Infix(ast::InfixExpression {
            left: Box::new(left),
            operator,
            right: Box::new(right),
        }))
    }

    fn parse_grouped_expression(&mut self) -> Result<ast::Expression, MonkeyError> {
        self.next_token();

        let exp = self.parse_expression(Precedence::Lowest)?;
        self.expect_peek(token::TokenType::RPAREN)?;
        Ok(exp)
    }

    fn parse_if_expression(&mut self) -> Result<ast::Expression, MonkeyError> {
        self.expect_peek(token::TokenType::LPAREN)?;
        self.next_token(); // skip '('

        let condition = self.parse_expression(Precedence::Lowest)?;

        self.expect_peek(token::TokenType::RPAREN)?;

        self.expect_peek(token::TokenType::LBRACE)?;

        let block = self.parse_block_statements()?;

        let mut if_exp = ast::IfExpression {
            condition: Box::new(condition),
            consequence: block,
            alternative: None,
        };

        if self.peek_token_is(token::TokenType::ELSE) {
            self.next_token();

            self.expect_peek(token::TokenType::LBRACE)?;

            if_exp.alternative = Some(self.parse_block_statements()?);
        }

        Ok(ast::Expression::If(if_exp))
    }

    fn parse_function_literal(&mut self) -> Result<ast::Expression, MonkeyError> {
        self.expect_peek(token::TokenType::LPAREN)?;

        let parameters = self.parse_function_parameters()?;

        self.expect_peek(token::TokenType::LBRACE)?;

        let body = self.parse_block_statements()?;

        Ok(ast::Expression::Fucntion(ast::FunctionLiteral {
            parameters: parameters,
            body: body,
        }))
    }

    fn parse_call_expression(
        &mut self,
        function: ast::Expression,
    ) -> Result<ast::Expression, MonkeyError> {
        self.next_token();

        let args = self.parse_call_arguments()?;

        Ok(ast::Expression::Call(ast::CallExpression {
            function: Box::new(function),
            arguments: args,
        }))
    }

    // when this method is called, cur_token is '{',
    // after this method returned, cur_token is '}'.
    fn parse_block_statements(&mut self) -> Result<ast::BlockStatement, MonkeyError> {
        self.next_token();

        let mut stmts = vec![];

        while !self.cur_token_is(token::TokenType::RBRACE)
            && !self.cur_token_is(token::TokenType::EOF)
        {
            let stmt = self.parse_statement()?;
            stmts.push(stmt);
            self.next_token();
        }

        Ok(ast::BlockStatement { statements: stmts })
    }

    // when this method is called, peek_token is '(',
    // after the method returned, cur_token is ')'.
    fn parse_function_parameters(&mut self) -> Result<Vec<ast::Identifier>, MonkeyError> {
        if self.peek_token_is(token::TokenType::RPAREN) {
            self.next_token();
            return Ok(vec![]);
        }
        self.next_token();

        let mut identifiers = vec![];

        let ident = ast::Identifier {
            value: self.cur_token.literal().clone(),
        };
        identifiers.push(ident);

        while self.peek_token_is(token::TokenType::COMMA) {
            self.next_token(); // after call, cur_token is ','
            self.next_token(); // skip ','
            let ident = ast::Identifier {
                value: self.cur_token.literal().clone(),
            };
            identifiers.push(ident);
        }

        self.expect_peek(token::TokenType::RPAREN)?;

        Ok(identifiers)
    }

    fn parse_call_arguments(&mut self) -> Result<Vec<ast::Expression>, MonkeyError> {
        let exps = self.parse_expressions(token::TokenType::RPAREN)?;
        Ok(exps)
    }

    fn parse_index_expression(
        &mut self,
        left: ast::Expression,
    ) -> Result<ast::Expression, MonkeyError> {
        self.next_token(); // drive cur_token to '['
        self.next_token(); // skip '['

        let index = self.parse_expression(Precedence::Lowest)?;

        self.expect_peek(token::TokenType::RBRACKET)?;

        Ok(ast::Expression::Index(ast::IndexExpression {
            left: Box::new(left),
            index: Box::new(index),
        }))
    }

    fn parse_expressions(
        &mut self,
        end: token::TokenType,
    ) -> Result<Vec<ast::Expression>, MonkeyError> {
        let mut exps = vec![];

        if self.peek_token_is(end) {
            self.next_token();
            return Ok(exps);
        }
        self.next_token();
        exps.push(self.parse_expression(Precedence::Lowest)?);

        while self.peek_token_is(token::TokenType::COMMA) {
            self.next_token();
            self.next_token();
            exps.push(self.parse_expression(Precedence::Lowest)?);
        }

        self.expect_peek(end)?;

        Ok(exps)
    }

    fn next_token(&mut self) {
        self.cur_token = self.peek_token.clone();
        self.peek_token = self.lexer.next_token();
    }

    fn cur_token_is(&self, typ: token::TokenType) -> bool {
        self.cur_token.typ() == typ
    }

    fn peek_token_is(&self, typ: token::TokenType) -> bool {
        self.peek_token.typ() == typ
    }

    fn expect_peek(&mut self, typ: token::TokenType) -> Result<(), MonkeyError> {
        if self.peek_token.typ() == typ {
            self.next_token();
            Ok(())
        } else {
            Err(MonkeyError::TypeError {
                expected: typ.to_string(),
                actual: self.peek_token.typ().to_string(),
            })
        }
    }

    fn peek_token_precedence(&self) -> Precedence {
        Precedence::from_token(self.peek_token.typ())
    }

    fn register_prefix_fn(
        &mut self,
        typ: token::TokenType,
        f: fn(&mut Parser) -> Result<ast::Expression, MonkeyError>,
    ) {
        self.prefix_parse_fns.insert(typ, f);
    }

    fn register_infix_fn(
        &mut self,
        typ: token::TokenType,
        f: fn(&mut Parser, ast::Expression) -> Result<ast::Expression, MonkeyError>,
    ) {
        self.infix_parse_fns.insert(typ, f);
    }
}

#[cfg(test)]
mod tests {

    use super::*;
    use crate::ast::{self, Identifier, Program};

    #[test]
    fn test_parse_program() {
        let tests = vec![
            ("let x = 5;", "x", "5"),
            ("let y = true;", "y", "true"),
            ("let foobar = y;", "foobar", "y"),
            // ("let 12345", "", ""), // for check errors
        ];

        for (input, expected_identifier, expected_value) in tests {
            let program = parse_program(input);

            assert_eq!(
                program.statements.len(),
                1,
                "program.statements does not contain 1 statement. got={}",
                program.statements.len()
            );

            match &program.statements[0] {
                ast::Statement::Let(stmt) => {
                    test_let_statement(stmt, expected_identifier, expected_value)
                }
                _ => {
                    panic!("unexpected type")
                }
            }
        }
    }

    #[test]
    fn test_return_statements() {
        let tests = vec![("return 5; return a;", 2)];

        for (input, stmts_count) in tests {
            let program = parse_program(input);

            assert_eq!(program.statements.len(), stmts_count);

            for stmt in program.statements {
                match &stmt {
                    ast::Statement::Return(ret_stmt) => {
                        test_return_statement(ret_stmt);
                    }
                    _ => panic!("unexpected type"),
                }
            }
        }
    }

    #[test]
    fn test_string() {
        let program = ast::Program {
            statements: vec![ast::Statement::Let(ast::LetStatement {
                name: ast::Identifier {
                    value: "my_var".to_string(),
                },
                value: ast::Expression::Ident(ast::Identifier {
                    value: "another_var".to_string(),
                }),
            })],
        };

        assert_eq!(program.to_string(), "let my_var = another_var;")
    }

    #[test]
    fn test_identifier_expression() {
        let input = "foobar";
        let program = parse_program(input);

        assert_eq!(program.statements.len(), 1);

        assert_eq!(
            program.statements[0],
            ast::Statement::Expression(ast::ExpressionStatement {
                expression: ast::Expression::Ident(ast::Identifier {
                    value: "foobar".to_string(),
                })
            })
        )
    }

    #[test]
    fn test_integer_expression() {
        let input = "123;";
        let program = parse_program(input);

        assert_eq!(program.statements.len(), 1);

        assert_eq!(
            program.statements[0],
            ast::Statement::Expression(ast::ExpressionStatement {
                expression: ast::Expression::Int(ast::Integer::new(123))
            })
        )
    }

    #[test]
    fn test_parse_prefix_expression_with_integer() {
        let tests = vec![
            ("!5", "!".to_string(), 5_i64),
            ("-15", "-".to_string(), 15_i64),
        ];

        for (input, operator, val) in tests {
            let program = parse_program(input);

            assert_eq!(program.statements.len(), 1);

            assert_eq!(
                program.statements[0],
                ast::Statement::Expression(ast::ExpressionStatement {
                    expression: ast::Expression::Prefix(ast::PrefixExpression {
                        operator,
                        right: Box::new(ast::Expression::Int(ast::Integer::new(val)))
                    })
                })
            )
        }
    }

    #[test]
    fn test_parse_prefix_expression_with_boolean() {
        let tests = vec![
            ("!true", "!".to_string(), true),
            ("!false", "!".to_string(), false),
        ];

        for (input, operator, val) in tests {
            let program = parse_program(input);

            assert_eq!(program.statements.len(), 1);

            assert_eq!(
                program.statements[0],
                ast::Statement::Expression(ast::ExpressionStatement {
                    expression: ast::Expression::Prefix(ast::PrefixExpression {
                        operator,
                        right: Box::new(ast::Expression::Bool(ast::Boolean { value: val }))
                    })
                })
            )
        }
    }

    #[test]
    fn test_parse_infix_expression() {
        let tests = vec![
            ("5 + 5;", 5, "+", 5),
            ("5 - 5;", 5, "-", 5),
            ("5 * 5;", 5, "*", 5),
            ("5 / 5;", 5, "/", 5),
            ("5 > 5;", 5, ">", 5),
            ("5 < 5;", 5, "<", 5),
            ("5 == 5;", 5, "==", 5),
            ("5 != 5;", 5, "!=", 5),
        ];

        for (input, left_val, operator, right_val) in tests {
            let program = parse_program(input);

            assert_eq!(
                program.statements[0],
                ast::Statement::Expression(ast::ExpressionStatement {
                    expression: ast::Expression::Infix(ast::InfixExpression {
                        left: Box::new(ast::Expression::Int(ast::Integer::new(left_val as i64))),
                        operator: operator.to_string(),
                        right: Box::new(ast::Expression::Int(ast::Integer::new(right_val as i64)))
                    })
                })
            );
        }
    }

    #[test]
    fn test_operator_precedence() {
        let tests = vec![
            ("-a * b", "((-a) * b)"),
            ("!-a", "(!(-a))"),
            ("a + b + c", "((a + b) + c)"),
            ("a + b - c", "((a + b) - c)"),
            ("a * b * c", "((a * b) * c)"),
            ("a * b / c", "((a * b) / c)"),
            ("a + b / c", "(a + (b / c))"),
            ("a + b * c + d / e - f", "(((a + (b * c)) + (d / e)) - f)"),
            ("3 + 4; -5 * 5", "(3 + 4)((-5) * 5)"),
            ("5 > 4 == 3 < 4", "((5 > 4) == (3 < 4))"),
            ("5 < 4 != 3 > 4", "((5 < 4) != (3 > 4))"),
            (
                "3 + 4 * 5 == 3 * 1 + 4 * 5",
                "((3 + (4 * 5)) == ((3 * 1) + (4 * 5)))",
            ),
            ("true", "true"),
            ("false", "false"),
            ("3 > 5 == false", "((3 > 5) == false)"),
            ("3 < 5 == true", "((3 < 5) == true)"),
            // with parenthes
            ("1 + (2 + 3) + 4", "((1 + (2 + 3)) + 4)"),
            ("(5 + 5) * 2", "((5 + 5) * 2)"),
            ("2 / (5 + 5)", "(2 / (5 + 5))"),
            ("-(5 + 5)", "(-(5 + 5))"),
            ("!(true == true)", "(!(true == true))"),
        ];

        for (input, expected) in tests {
            let program = parse_program(input);

            let actual = program.to_string();
            assert_eq!(actual, expected, "expected={}, got={}", expected, actual);
        }
    }

    #[test]
    fn test_boolean_expression() {
        let tests = vec![("true;", true), ("false;", false)];

        for (input, _expected) in tests {
            let program = parse_program(input);

            assert_eq!(program.statements.len(), 1);
            // Note: Boolean expressions are not yet implemented in the AST
            // This test will need to be updated when BooleanLiteral is added
        }
    }

    #[test]
    fn test_if_expression() {
        let input = "if (x < y) { x }";
        let program = parse_program(input);

        assert_eq!(program.statements.len(), 1);
        // Check pretty-printed output (single parens, trailing blank line from block)
        assert_eq!(program.to_string(), "if (x < y) {\nx\n}");

        // Check AST shape
        let expected = ast::Statement::Expression(ast::ExpressionStatement {
            expression: ast::Expression::If(ast::IfExpression {
                condition: Box::new(ast::Expression::Infix(ast::InfixExpression {
                    left: Box::new(ast::Expression::Ident(ast::Identifier {
                        value: "x".to_string(),
                    })),
                    operator: "<".to_string(),
                    right: Box::new(ast::Expression::Ident(ast::Identifier {
                        value: "y".to_string(),
                    })),
                })),
                consequence: ast::BlockStatement {
                    statements: vec![ast::Statement::Expression(ast::ExpressionStatement {
                        expression: ast::Expression::Ident(ast::Identifier {
                            value: "x".to_string(),
                        }),
                    })],
                },
                alternative: None,
            }),
        });

        assert_eq!(program.statements[0], expected);
    }

    #[test]
    fn test_function_literal() {
        let input = "fn(x, y) { x + y; }";
        let program = parse_program(input);

        assert_eq!(program.statements.len(), 1);

        // Check AST structure
        let expected = ast::Program {
            statements: vec![ast::Statement::Expression(ast::ExpressionStatement {
                expression: ast::Expression::Fucntion(ast::FunctionLiteral {
                    parameters: vec![
                        ast::Identifier {
                            value: "x".to_string(),
                        },
                        ast::Identifier {
                            value: "y".to_string(),
                        },
                    ],
                    body: ast::BlockStatement {
                        statements: vec![ast::Statement::Expression(ast::ExpressionStatement {
                            expression: ast::Expression::Infix(ast::InfixExpression {
                                left: Box::new(ast::Expression::Ident(ast::Identifier {
                                    value: "x".to_string(),
                                })),
                                operator: "+".to_string(),
                                right: Box::new(ast::Expression::Ident(ast::Identifier {
                                    value: "y".to_string(),
                                })),
                            }),
                        })],
                    },
                }),
            })],
        };

        assert_eq!(program, expected);
        // Pretty-print should include a trailing blank line from BlockStatement and FunctionLiteral
        assert_eq!(program.to_string(), "fn(x, y) {\n(x + y)\n}");
    }

    #[test]
    fn test_call_expression() {
        let input = "add(1, 2 * 3, 4 + 5);";
        let expected = ast::Program {
            statements: vec![ast::Statement::Expression(ast::ExpressionStatement {
                expression: ast::Expression::Call(ast::CallExpression {
                    function: Box::new(ast::Expression::Ident(ast::Identifier {
                        value: "add".to_string(),
                    })),
                    arguments: vec![
                        ast::Expression::Int(ast::Integer::new(1)),
                        ast::Expression::Infix(ast::InfixExpression {
                            left: Box::new(ast::Expression::Int(ast::Integer::new(2))),
                            operator: "*".to_string(),
                            right: Box::new(ast::Expression::Int(ast::Integer::new(3))),
                        }),
                        ast::Expression::Infix(ast::InfixExpression {
                            left: Box::new(ast::Expression::Int(ast::Integer::new(4))),
                            operator: "+".to_string(),
                            right: Box::new(ast::Expression::Int(ast::Integer::new(5))),
                        }),
                    ],
                }),
            })],
        };
        let program = parse_program(input);

        assert_eq!(program.statements.len(), 1);
        assert_eq!(program, expected);
    }

    #[test]
    fn test_array() {
        let input = "[1, 2 * 2, 3 + 3]";
        let expected = ast::Program {
            statements: vec![ast::Statement::Expression(ast::ExpressionStatement {
                expression: ast::Expression::Array(ast::ArrayLiteral {
                    elems: vec![
                        ast::Expression::Int(ast::Integer::new(1)),
                        ast::Expression::Infix(ast::InfixExpression {
                            left: Box::new(ast::Expression::Int(ast::Integer::new(2))),
                            operator: "*".to_string(),
                            right: Box::new(ast::Expression::Int(ast::Integer::new(2))),
                        }),
                        ast::Expression::Infix(ast::InfixExpression {
                            left: Box::new(ast::Expression::Int(ast::Integer::new(3))),
                            operator: "+".to_string(),
                            right: Box::new(ast::Expression::Int(ast::Integer::new(3))),
                        }),
                    ],
                }),
            })],
        };
        let program = parse_program(input);

        assert_eq!(program, expected);
    }

    #[test]
    fn test_array_index() {
        let input = "arr[1 + 1]";
        let expected = ast::Program {
            statements: vec![ast::Statement::Expression(ast::ExpressionStatement {
                expression: ast::Expression::Index(ast::IndexExpression {
                    left: Box::new(ast::Expression::Ident(Identifier {
                        value: "arr".to_string(),
                    })),
                    index: Box::new(ast::Expression::Infix(ast::InfixExpression {
                        left: Box::new(ast::Expression::Int(ast::Integer::new(1))),
                        operator: "+".to_string(),
                        right: Box::new(ast::Expression::Int(ast::Integer::new(1))),
                    })),
                }),
            })],
        };
        let program = parse_program(input);

        assert_eq!(program, expected);
    }

    fn test_let_statement(stmt: &ast::LetStatement, name: &str, value: &str) {
        assert_eq!(stmt.name.value, name);

        match &stmt.value {
            ast::Expression::Ident(ident) => {
                assert_eq!(ident.value, value);
            }
            ast::Expression::Int(int) => {
                assert_eq!(int.to_string(), value);
            }
            ast::Expression::Bool(boolean) => {
                assert_eq!(boolean.value.to_string(), value);
            }
            _ => {
                panic!("unexpected value type: {:?}", stmt.value);
            }
        }
    }

    fn test_return_statement(stmt: &ast::ReturnStatement) {
        // Verify that return statement has an expression
        match &stmt.expression {
            ast::Expression::Ident(_) => {}
            ast::Expression::Int(_) => {}
            ast::Expression::Bool(_) => {}
            ast::Expression::Prefix(_) => {}
            ast::Expression::Infix(_) => {}
            _ => {
                panic!(
                    "unexpected expression type in return statement: {:?}",
                    stmt.expression
                );
            }
        }
    }

    fn parse_program(input: &str) -> Program {
        let lexer = lexer::Lexer::new(input);
        let mut parser = Parser::new(lexer);
        parser.parse_program().unwrap()
    }
}
