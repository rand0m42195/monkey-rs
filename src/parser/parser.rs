use crate::{ast, lexer, token};

pub struct Parser {
    lexer: lexer::Lexer,
    cur_token: token::Token,
    peek_token: token::Token,
    errors: Vec<String>,
}

impl Parser {
    pub fn new(lexer: lexer::Lexer) -> Parser {
        let mut parser = Parser {
            lexer,
            cur_token: token::Token::new(token::TokenType::ILLEGAL, "".to_string()),
            peek_token: token::Token::new(token::TokenType::ILLEGAL, "".to_string()),
            errors: vec![],
        };

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
            _ => None,
        }
    }

    fn parse_let_statement(&mut self) -> Option<ast::LetStatement> {
        let tok = self.cur_token.clone();
        if !self.expect_peek(token::TokenType::IDENT) {
            return None;
        }

        let ident = ast::Identifier {
            token: self.cur_token.clone(),
            value: self.cur_token.literal().clone(),
        };

        if !self.expect_peek(token::TokenType::ASSIGN) {
            return None;
        }
        // now self.cur_token is '=', skip it.
        self.next_token();

        // let exp = self.parse_expression();
        // if exp.is_none() {
        //     return None;
        // }

        // if !self.expect_peek(token::TokenType::SEMICOLON) {
        //     return None;
        // }

        while !self.cur_token_is(token::TokenType::SEMICOLON) {
            self.next_token();
        }

        Some(ast::LetStatement {
            token: tok,
            name: ident,
            value: ast::ExpressionNode::Empty(),
        })
    }

    fn parse_return_statement(&mut self) -> Option<ast::ReturnStatement> {
        let tok = self.cur_token.clone();
        self.next_token();

        while !self.cur_token_is(token::TokenType::SEMICOLON) {
            self.next_token();
        }

        Some(ast::ReturnStatement {
            token: tok,
            expression: ast::ExpressionNode::Empty(),
        })
    }

    // TODO
    fn parse_expression(&mut self) -> Option<ast::ExpressionNode> {
        let prefix = self.prefix_parse_fn();
        if prefix.is_none() {
            return None;
        }

        Some(prefix.unwrap())
    }

    fn prefix_parse_fn(&mut self) -> Option<ast::ExpressionNode> {
        match self.cur_token.typ() {
            token::TokenType::IDENT => Some(ast::ExpressionNode::Identifier(ast::Identifier {
                token: self.cur_token.clone(),
                value: self.cur_token.literal().clone(),
            })),
            token::TokenType::INT => Some(ast::ExpressionNode::Integer(ast::IntegerLiteral {
                token: self.cur_token.clone(),
                value: self.cur_token.literal().clone(),
            })),

            _ => None,
        }
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
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{
        ast::{self, Node},
        lexer,
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

    fn test_let_statement(stmt: &ast::LetStatement, name: &str, value: &str) {
        assert_eq!(stmt.token_literal(), "let".to_string());
        assert_eq!(stmt.name.token_literal(), name.to_string());
        // TODO:
        // assert_eq!(stmt.value)
    }

    fn test_return_statement(stmt: &ast::ReturnStatement) {
        assert_eq!(stmt.token_literal(), "return".to_string());
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
