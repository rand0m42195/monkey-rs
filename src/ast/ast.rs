use crate::token;

pub trait Node {
    fn token_literal(&self) -> String;
}

pub trait Statement: Node {
    fn statement_node(&self) {
        // Default implementation - can be overridden
    }
}

pub trait Expression: Node {
    fn expression_node(&self) {
        // Default implementation - can be overridden
    }
}

#[derive(Debug, Clone)]
pub enum StatementNode {
    Let(LetStatement),
    Return(ReturnStatement),
    // Expression(ExpressionStatement),
    // Block(BlockStatement),
}

#[derive(Debug, Clone)]
pub enum ExpressionNode {
    Empty(),
    Identifier(Identifier),
    Integer(IntegerLiteral),
    // Boolean(BooleanLiteral),
}

// Use Box<dyn Statement> for trait objects
pub struct Program {
    pub statements: Vec<StatementNode>,
}

impl Node for Program {
    fn token_literal(&self) -> String {
        if self.statements.is_empty() {
            "".to_string()
        } else {
            // self.statements[0].token_literal()
            // TODO:
            "".to_string()
        }
    }
}

#[derive(Debug, Clone)]
pub struct LetStatement {
    pub token: token::Token,
    pub name: Identifier,
    pub value: ExpressionNode,
}

impl Node for LetStatement {
    fn token_literal(&self) -> String {
        self.token.literal().clone()
    }
}

impl Statement for LetStatement {}

#[derive(Debug, Clone)]
pub struct Identifier {
    pub token: token::Token,
    pub value: String,
}

impl Node for Identifier {
    fn token_literal(&self) -> String {
        self.token.literal().clone()
    }
}

impl Expression for Identifier {}

#[derive(Debug, Clone)]
pub struct IntegerLiteral {
    pub token: token::Token,
    pub value: String,
}

impl Node for IntegerLiteral {
    fn token_literal(&self) -> String {
        self.token.literal().clone()
    }
}

impl Expression for IntegerLiteral {}

#[derive(Debug, Clone)]
pub struct ReturnStatement {
    pub token: token::Token,
    pub expression: ExpressionNode,
}

impl Node for ReturnStatement {
    fn token_literal(&self) -> String {
        self.token.literal().clone()
    }
}

impl Statement for ReturnStatement {}
