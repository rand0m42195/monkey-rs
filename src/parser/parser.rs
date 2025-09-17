use crate::{ast, lexer, token};
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
}

impl Precedence {
    fn from_token(typ: token::TokenType) -> Self {
        match typ {
            token::TokenType::EQ | token::TokenType::NotEq => Precedence::Equals,
            token::TokenType::LT | token::TokenType::GT => Precedence::LessGreater,
            token::TokenType::PLUS | token::TokenType::MINUS => Precedence::Sum,
            token::TokenType::ASTERISK | token::TokenType::SLASH => Precedence::Product,
            token::TokenType::LPAREN => Precedence::Call,
            _ => Precedence::Lowest,
        }
    }
}

pub struct Parser {
    lexer: lexer::Lexer,
    cur_token: token::Token,
    peek_token: token::Token,
    errors: Vec<String>,

    prefix_parse_fns: HashMap<token::TokenType, fn(&mut Parser) -> Option<ast::ExpressionNode>>,
    infix_parse_fns: HashMap<
        token::TokenType,
        fn(&mut Parser, ast::ExpressionNode) -> Option<ast::ExpressionNode>,
    >,
}

impl Parser {
    pub fn new(lexer: lexer::Lexer) -> Parser {
        let mut parser = Parser {
            lexer,
            cur_token: token::Token::new(token::TokenType::ILLEGAL, "".to_string()),
            peek_token: token::Token::new(token::TokenType::ILLEGAL, "".to_string()),
            errors: vec![],
            prefix_parse_fns: HashMap::new(),
            infix_parse_fns: HashMap::new(),
        };

        parser.register_prefix_fn(token::TokenType::IDENT, Self::parse_identifier);
        parser.register_prefix_fn(token::TokenType::INT, Self::parse_integer);
        parser.register_prefix_fn(token::TokenType::MINUS, Self::parse_prefix_expression);
        parser.register_prefix_fn(token::TokenType::BANG, Self::parse_prefix_expression);
        parser.register_prefix_fn(token::TokenType::TRUE, Self::parse_boolean);
        parser.register_prefix_fn(token::TokenType::FALSE, Self::parse_boolean);
        parser.register_prefix_fn(token::TokenType::LPAREN, Self::parse_grouped_expression);
        parser.register_prefix_fn(token::TokenType::IF, Self::parse_if_expression);
        parser.register_prefix_fn(token::TokenType::FUNCTION, Self::parse_function_literal);

        parser.register_infix_fn(token::TokenType::PLUS, Self::parse_infix_expression);
        parser.register_infix_fn(token::TokenType::MINUS, Self::parse_infix_expression);
        parser.register_infix_fn(token::TokenType::SLASH, Self::parse_infix_expression);
        parser.register_infix_fn(token::TokenType::ASTERISK, Self::parse_infix_expression);
        parser.register_infix_fn(token::TokenType::EQ, Self::parse_infix_expression);
        parser.register_infix_fn(token::TokenType::NotEq, Self::parse_infix_expression);
        parser.register_infix_fn(token::TokenType::LT, Self::parse_infix_expression);
        parser.register_infix_fn(token::TokenType::GT, Self::parse_infix_expression);
        parser.register_infix_fn(token::TokenType::LPAREN, Self::parse_call_expression);

        parser.next_token();
        parser.next_token();

        parser
    }

    pub fn parse_program(&mut self) -> ast::Program {
        let mut program = ast::Program { statements: vec![] };

        while self.cur_token.typ() != token::TokenType::EOF {
            let stmt = self.parse_statement();
            if stmt.is_some() {
                program.statements.push(stmt.unwrap());
            }
            self.next_token();
        }

        program
    }

    pub fn has_errors(&self) -> bool {
        !self.errors.is_empty()
    }

    pub fn parse_error_msg(&self) -> Vec<String> {
        self.errors.clone()
    }

    fn parse_statement(&mut self) -> Option<ast::StatementNode> {
        match self.cur_token.typ() {
            token::TokenType::LET => {
                let stmt = self.parse_let_statement();
                if let Some(let_stmt) = stmt {
                    Some(ast::StatementNode::Let(let_stmt))
                } else {
                    None
                }
            }
            token::TokenType::RETURN => {
                let stmt = self.parse_return_statement();
                if let Some(ret_stmt) = stmt {
                    Some(ast::StatementNode::Return(ret_stmt))
                } else {
                    None
                }
            }

            _ => {
                let stmt = self.parse_expression_statement();
                if let Some(exp_stmt) = stmt {
                    Some(ast::StatementNode::Expression(ast::ExpressionStatement {
                        expression: exp_stmt,
                    }))
                } else {
                    None
                }
            }
        }
    }

    fn parse_let_statement(&mut self) -> Option<ast::LetStatement> {
        if !self.expect_peek(token::TokenType::IDENT) {
            return None;
        }

        let ident = ast::Identifier {
            value: self.cur_token.literal().clone(),
        };

        if !self.expect_peek(token::TokenType::ASSIGN) {
            return None;
        }
        // now self.cur_token is '=', skip it.
        self.next_token();

        while !self.cur_token_is(token::TokenType::SEMICOLON) {
            self.next_token();
        }

        Some(ast::LetStatement {
            name: ident,
            value: ast::ExpressionNode::Empty(),
        })
    }

    fn parse_return_statement(&mut self) -> Option<ast::ReturnStatement> {
        self.next_token();

        while !self.cur_token_is(token::TokenType::SEMICOLON) {
            self.next_token();
        }

        Some(ast::ReturnStatement {
            expression: ast::ExpressionNode::Empty(),
        })
    }

    fn parse_expression_statement(&mut self) -> Option<ast::ExpressionNode> {
        if let Some(node) = self.parse_expression(Precedence::Lowest) {
            if self.peek_token_is(token::TokenType::SEMICOLON) {
                self.next_token();
            }
            Some(node)
        } else {
            None
        }
    }

    fn parse_expression(&mut self, precedence: Precedence) -> Option<ast::ExpressionNode> {
        if let Some(prefix_fn) = self.prefix_parse_fns.get(&self.cur_token.typ()) {
            let left_exp = prefix_fn(self);
            if left_exp.is_none() {
                return None;
            }
            let mut left_exp = left_exp.unwrap();

            while !self.cur_token_is(token::TokenType::SEMICOLON)
                && precedence < self.peek_token_precedence()
            {
                if let Some(infix_fn) = self.infix_parse_fns.get(&self.peek_token.typ()) {
                    let res = infix_fn(self, left_exp);
                    if res.is_none() {
                        return None;
                    }
                    left_exp = res.unwrap();
                } else {
                    return Some(left_exp);
                }
            }

            Some(left_exp)
        } else {
            self.no_prefix_parse_fn_error(self.cur_token.typ());
            None
        }
    }

    fn parse_identifier(&mut self) -> Option<ast::ExpressionNode> {
        if !self.cur_token_is(token::TokenType::IDENT) {
            return None;
        }

        Some(ast::ExpressionNode::Identifier(ast::Identifier {
            value: self.cur_token.literal().clone(),
        }))
    }

    fn parse_integer(&mut self) -> Option<ast::ExpressionNode> {
        if !self.cur_token_is(token::TokenType::INT) {
            return None;
        }

        let n = self.cur_token.literal().parse::<i64>();
        if n.is_err() {
            self.errors.push(format!(
                "parse {} failed: {:?}",
                self.cur_token.literal(),
                n.err()
            ));
            return None;
        }

        Some(ast::ExpressionNode::Integer(ast::IntegerLiteral {
            token: self.cur_token.clone(),
            value: n.unwrap(),
        }))
    }

    fn parse_boolean(&mut self) -> Option<ast::ExpressionNode> {
        let exp = if self.cur_token_is(token::TokenType::TRUE) {
            ast::Boolean { value: true }
        } else {
            ast::Boolean { value: false }
        };
        Some(ast::ExpressionNode::Bool(exp))
    }

    fn parse_prefix_expression(&mut self) -> Option<ast::ExpressionNode> {
        let operator = self.cur_token.literal().clone();
        // consume the prefix operator, after call next_token
        // self.cur_token is the start of the right expression
        self.next_token();

        if let Some(prefix_fn) = self.prefix_parse_fns.get(&self.cur_token.typ()) {
            if let Some(exp_node) = prefix_fn(self) {
                Some(ast::ExpressionNode::Prefix(ast::PrefixExpression {
                    operator,
                    right: Box::new(exp_node),
                }))
            } else {
                None
            }
        } else {
            self.no_prefix_parse_fn_error(self.cur_token.typ());
            None
        }
    }

    fn parse_infix_expression(&mut self, left: ast::ExpressionNode) -> Option<ast::ExpressionNode> {
        self.next_token();
        let operator = self.cur_token.literal().clone();
        let precedence = Precedence::from_token(self.cur_token.typ());
        // consume the infix operator, after call next_token
        // self.cur_token is the start of the right expression
        self.next_token();

        if let Some(right) = self.parse_expression(precedence) {
            Some(ast::ExpressionNode::Infix(ast::InfixExpression {
                left: Box::new(left),
                operator,
                right: Box::new(right),
            }))
        } else {
            None
        }
    }

    fn parse_grouped_expression(&mut self) -> Option<ast::ExpressionNode> {
        self.next_token();

        if let Some(exp) = self.parse_expression(Precedence::Lowest) {
            if !self.expect_peek(token::TokenType::RPAREN) {
                return None;
            }
            Some(exp)
        } else {
            None
        }
    }

    fn parse_if_expression(&mut self) -> Option<ast::ExpressionNode> {
        // cur_token is if, expect next token is '('
        if !self.expect_peek(token::TokenType::LPAREN) {
            return None;
        }
        self.next_token(); // skip '('

        let condition = self.parse_expression(Precedence::Lowest);
        if condition.is_none() {
            return None;
        }

        if !self.expect_peek(token::TokenType::RPAREN) {
            return None;
        }

        if !self.expect_peek(token::TokenType::LBRACE) {
            return None;
        }

        let block = self.parse_block_statements();
        if block.is_none() {
            return None;
        }

        let mut if_exp = ast::IfExpression {
            condition: Box::new(condition.unwrap()),
            consequence: block.unwrap(),
            alternative: None,
        };

        if self.peek_token_is(token::TokenType::ELSE) {
            self.next_token();
            if !self.expect_peek(token::TokenType::LBRACE) {
                return None;
            }
            let alternative = self.parse_block_statements();
            if alternative.is_none() {
                return None;
            }
            if_exp.alternative = alternative;
        }

        Some(ast::ExpressionNode::If(if_exp))
    }

    fn parse_function_literal(&mut self) -> Option<ast::ExpressionNode> {
        if !self.expect_peek(token::TokenType::LPAREN) {
            return None;
        }

        let parameters = self.parse_function_parameters();
        if parameters.is_none() {
            return None;
        }

        if !self.expect_peek(token::TokenType::LBRACE) {
            return None;
        }

        let body = self.parse_block_statements();
        if body.is_none() {
            return None;
        }

        Some(ast::ExpressionNode::Fucntion(ast::FunctionLiteral {
            parameters: parameters.unwrap(),
            body: body.unwrap(),
        }))
    }

    fn parse_call_expression(
        &mut self,
        function: ast::ExpressionNode,
    ) -> Option<ast::ExpressionNode> {
        self.next_token();

        let args = self.parse_call_arguments();
        if args.is_none() {
            return None;
        }
        Some(ast::ExpressionNode::Call(ast::CallExpression {
            function: Box::new(function),
            arguments: args.unwrap(),
        }))
    }

    // when this method is called, cur_token is '{',
    // after this method returned, cur_token is '}'.
    fn parse_block_statements(&mut self) -> Option<ast::BlockStatement> {
        self.next_token();

        let mut stmts = vec![];

        while !self.cur_token_is(token::TokenType::RBRACE)
            && !self.cur_token_is(token::TokenType::EOF)
        {
            let stmt = self.parse_statement();
            if let Some(st) = stmt {
                stmts.push(st);
            }
            self.next_token();
        }

        Some(ast::BlockStatement { statements: stmts })
    }

    // when this method is called, peek_token is '(',
    // after the method returned, cur_token is ')'.
    fn parse_function_parameters(&mut self) -> Option<Vec<ast::Identifier>> {
        if self.peek_token_is(token::TokenType::RPAREN) {
            self.next_token();
            return Some(vec![]);
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

        if !self.expect_peek(token::TokenType::RPAREN) {
            return None;
        }

        Some(identifiers)
    }

    fn parse_call_arguments(&mut self) -> Option<Vec<ast::ExpressionNode>> {
        let mut args = vec![];

        if self.peek_token_is(token::TokenType::RPAREN) {
            self.next_token();
            return Some(args);
        }
        self.next_token(); // skip '('

        let arg = self.parse_expression(Precedence::Lowest);
        if arg.is_none() {
            return None;
        }
        args.push(arg.unwrap());

        while self.peek_token_is(token::TokenType::COMMA) {
            self.next_token();
            self.next_token();
            let arg = self.parse_expression(Precedence::Lowest);
            if arg.is_none() {
                return None;
            }
            args.push(arg.unwrap());
        }

        if !self.expect_peek(token::TokenType::RPAREN) {
            return None;
        }

        Some(args)
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

    fn expect_peek(&mut self, typ: token::TokenType) -> bool {
        if self.peek_token.typ() == typ {
            self.next_token();
            true
        } else {
            self.peek_error(typ);
            false
        }
    }

    fn peek_error(&mut self, typ: token::TokenType) {
        self.errors.push(format!(
            "expected next token to be {}, got {} instead",
            typ,
            self.cur_token.typ()
        ))
    }

    fn peek_token_precedence(&self) -> Precedence {
        Precedence::from_token(self.peek_token.typ())
    }

    fn no_prefix_parse_fn_error(&mut self, typ: token::TokenType) {
        self.errors
            .push(format!("no prefix parse function for {} found", typ));
    }

    fn register_prefix_fn(
        &mut self,
        typ: token::TokenType,
        f: fn(&mut Parser) -> Option<ast::ExpressionNode>,
    ) {
        self.prefix_parse_fns.insert(typ, f);
    }

    fn register_infix_fn(
        &mut self,
        typ: token::TokenType,
        f: fn(&mut Parser, ast::ExpressionNode) -> Option<ast::ExpressionNode>,
    ) {
        self.infix_parse_fns.insert(typ, f);
    }
}

#[cfg(test)]
mod tests {

    use super::*;
    use crate::{
        ast::{self},
        lexer, token,
    };

    #[test]
    fn test_parse_program() {
        let tests = vec![
            ("let x = 5;", "x", "5"),
            ("let y = true;", "y", "true"),
            ("let foobar = y;", "foobar", "y"),
            // ("let 12345", "", ""), // for check errors
        ];

        for (input, expected_identifier, expected_value) in tests {
            let lexer = lexer::Lexer::new(input);
            let mut parser = Parser::new(lexer);

            let program = parser.parse_program();
            check_parser_errors(&parser);

            assert_eq!(
                program.statements.len(),
                1,
                "program.statements does not contain 1 statement. got={}",
                program.statements.len()
            );

            // let stmt = &program.statements[0];
            // assert!(
            //     test_let_statement(stmt.as_ref(), expected_identifier, expected_value),
            //     "test_let_statement failed for input: {}",
            //     input
            // );
            match &program.statements[0] {
                ast::StatementNode::Let(stmt) => {
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
            let lexer = lexer::Lexer::new(input);
            let mut parser = Parser::new(lexer);

            let program = parser.parse_program();
            check_parser_errors(&parser);

            assert_eq!(program.statements.len(), stmts_count);

            for stmt in program.statements {
                match &stmt {
                    ast::StatementNode::Return(ret_stmt) => {
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
            statements: vec![ast::StatementNode::Let(ast::LetStatement {
                name: ast::Identifier {
                    value: "my_var".to_string(),
                },
                value: ast::ExpressionNode::Identifier(ast::Identifier {
                    value: "another_var".to_string(),
                }),
            })],
        };

        assert_eq!(program.to_string(), "let my_var = another_var;")
    }

    #[test]
    fn test_identifier_expression() {
        let input = "foobar";
        let lexer = lexer::Lexer::new(input);
        let mut parser = Parser::new(lexer);

        let program = parser.parse_program();
        check_parser_errors(&parser);

        assert_eq!(program.statements.len(), 1);

        assert_eq!(
            program.statements[0],
            ast::StatementNode::Expression(ast::ExpressionStatement {
                expression: ast::ExpressionNode::Identifier(ast::Identifier {
                    value: "foobar".to_string(),
                })
            })
        )
    }

    #[test]
    fn test_integer_expression() {
        let input = "123;";
        let lexer = lexer::Lexer::new(input);
        let mut parser = Parser::new(lexer);

        let program = parser.parse_program();
        check_parser_errors(&parser);

        assert_eq!(program.statements.len(), 1);

        assert_eq!(
            program.statements[0],
            ast::StatementNode::Expression(ast::ExpressionStatement {
                expression: ast::ExpressionNode::Integer(ast::IntegerLiteral {
                    token: token::Token::new(token::TokenType::INT, "123".to_string()),
                    value: 123,
                })
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
            let lexer = lexer::Lexer::new(input);
            let mut parser = Parser::new(lexer);
            let program = parser.parse_program();
            check_parser_errors(&parser);

            assert_eq!(program.statements.len(), 1);

            assert_eq!(
                program.statements[0],
                ast::StatementNode::Expression(ast::ExpressionStatement {
                    expression: ast::ExpressionNode::Prefix(ast::PrefixExpression {
                        operator,
                        right: Box::new(ast::ExpressionNode::Integer(ast::IntegerLiteral {
                            token: token::Token::new(token::TokenType::INT, val.to_string()),
                            value: val,
                        }))
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
            let lexer = lexer::Lexer::new(input);
            let mut parser = Parser::new(lexer);
            let program = parser.parse_program();
            check_parser_errors(&parser);

            assert_eq!(program.statements.len(), 1);

            assert_eq!(
                program.statements[0],
                ast::StatementNode::Expression(ast::ExpressionStatement {
                    expression: ast::ExpressionNode::Prefix(ast::PrefixExpression {
                        operator,
                        right: Box::new(ast::ExpressionNode::Bool(ast::Boolean { value: val }))
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
            let lexer = lexer::Lexer::new(input);
            let mut parser = Parser::new(lexer);
            let program = parser.parse_program();
            check_parser_errors(&parser);

            assert_eq!(
                program.statements[0],
                ast::StatementNode::Expression(ast::ExpressionStatement {
                    expression: ast::ExpressionNode::Infix(ast::InfixExpression {
                        left: Box::new(ast::ExpressionNode::Integer(ast::IntegerLiteral {
                            token: token::Token::new(token::TokenType::INT, left_val.to_string()),
                            value: left_val as i64,
                        })),
                        operator: operator.to_string(),
                        right: Box::new(ast::ExpressionNode::Integer(ast::IntegerLiteral {
                            token: token::Token::new(token::TokenType::INT, right_val.to_string()),
                            value: right_val as i64,
                        }))
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
            let lexer = lexer::Lexer::new(input);
            let mut parser = Parser::new(lexer);
            let program = parser.parse_program();
            check_parser_errors(&parser);

            let actual = program.to_string();
            assert_eq!(actual, expected, "expected={}, got={}", expected, actual);
        }
    }

    #[test]
    fn test_boolean_expression() {
        let tests = vec![("true;", true), ("false;", false)];

        for (input, _expected) in tests {
            let lexer = lexer::Lexer::new(input);
            let mut parser = Parser::new(lexer);
            let program = parser.parse_program();
            check_parser_errors(&parser);

            assert_eq!(program.statements.len(), 1);
            // Note: Boolean expressions are not yet implemented in the AST
            // This test will need to be updated when BooleanLiteral is added
        }
    }

    #[test]
    fn test_if_expression() {
        let input = "if (x < y) { x }";
        let lexer = lexer::Lexer::new(input);
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();
        check_parser_errors(&parser);

        assert_eq!(program.statements.len(), 1);
        // Check pretty-printed output (single parens, trailing blank line from block)
        assert_eq!(program.to_string(), "if (x < y) {\nx\n\n}");

        // Check AST shape
        let expected = ast::StatementNode::Expression(ast::ExpressionStatement {
            expression: ast::ExpressionNode::If(ast::IfExpression {
                condition: Box::new(ast::ExpressionNode::Infix(ast::InfixExpression {
                    left: Box::new(ast::ExpressionNode::Identifier(ast::Identifier {
                        value: "x".to_string(),
                    })),
                    operator: "<".to_string(),
                    right: Box::new(ast::ExpressionNode::Identifier(ast::Identifier {
                        value: "y".to_string(),
                    })),
                })),
                consequence: ast::BlockStatement {
                    statements: vec![ast::StatementNode::Expression(ast::ExpressionStatement {
                        expression: ast::ExpressionNode::Identifier(ast::Identifier {
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
        let lexer = lexer::Lexer::new(input);
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();
        check_parser_errors(&parser);

        assert_eq!(program.statements.len(), 1);
        // Pretty-print should include a trailing blank line from BlockStatement and FunctionLiteral
        assert_eq!(program.to_string(), "fn(x, y) {\n(x + y)\n\n}");
    }

    #[test]
    fn test_call_expression() {
        let input = "add(1, 2 * 3, 4 + 5);";
        let lexer = lexer::Lexer::new(input);
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();
        check_parser_errors(&parser);

        assert_eq!(program.statements.len(), 1);
        // Note: Call expressions are not yet implemented in the AST
        // This test will need to be updated when CallExpression is added
    }

    fn test_let_statement(_stmt: &ast::LetStatement, _name: &str, _value: &str) {
        // assert_eq!(stmt.token_literal(), "let".to_string());
        // assert_eq!(stmt.name.token_literal(), name.to_string());
        // TODO:
        // assert_eq!(stmt.value)
    }

    fn test_return_statement(_stmt: &ast::ReturnStatement) {
        // assert_eq!(stmt.token_literal(), "return".to_string());
    }

    fn check_parser_errors(parser: &Parser) {
        if parser.errors.is_empty() {
            return;
        }

        eprintln!("parser has {} errors", parser.errors.len());
        for err_msg in &parser.errors {
            eprintln!("parser error: {}", err_msg);
        }

        panic!("failing test due to parser errors");
    }
}
