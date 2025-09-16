use std::fmt::Display;

use crate::token;

// pub trait Node {
//     fn token_literal(&self) -> String;
// }

// pub trait Statement: Node {
//     fn statement_node(&self) {
//         // Default implementation - can be overridden
//     }
// }

// pub trait Expression: Node {
//     fn expression_node(&self) {
//         // Default implementation - can be overridden
//     }
// }

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum StatementNode {
    Let(LetStatement),
    Return(ReturnStatement),
    Expression(ExpressionStatement),
    // Block(BlockStatement),
}

impl Display for StatementNode {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            StatementNode::Let(s) => {
                write!(f, "{}", s)
            }
            StatementNode::Return(s) => {
                write!(f, "{}", s)
            }
            StatementNode::Expression(s) => {
                write!(f, "{}", s)
            }
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ExpressionNode {
    Empty(),
    Identifier(Identifier),
    Integer(IntegerLiteral),
    Prefix(PrefixExpression),
    Infix(InfixExpression),
    Bool(Boolean),
    If(IfExpression),
    Fucntion(FunctionLiteral),
    Call(CallExpression),
}

impl Display for ExpressionNode {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ExpressionNode::Empty() => write!(f, "EMPTY"),
            ExpressionNode::Identifier(e) => write!(f, "{}", e),
            ExpressionNode::Integer(e) => write!(f, "{}", e),
            ExpressionNode::Prefix(e) => write!(f, "{}", e),
            ExpressionNode::Infix(e) => write!(f, "{}", e),
            ExpressionNode::Bool(e) => write!(f, "{}", e),
            ExpressionNode::If(e) => write!(f, "{}", e),
            ExpressionNode::Fucntion(e) => write!(f, "{}", e),
            ExpressionNode::Call(e) => write!(f, "{}", e),
        }
        // DO NOT USE this code, it will cause inifinite loop!
        // write!(f, "{}", self)
    }
}

pub struct Program {
    pub statements: Vec<StatementNode>,
}

impl Display for Program {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for stmt in &self.statements {
            write!(f, "{}", stmt)?;
        }

        Ok(())
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct LetStatement {
    pub name: Identifier,
    pub value: ExpressionNode,
}

impl Display for LetStatement {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "let {} = {};", self.name, self.value)
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ReturnStatement {
    pub expression: ExpressionNode,
}

impl Display for ReturnStatement {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "return {};", self.expression)
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ExpressionStatement {
    pub expression: ExpressionNode,
}

impl Display for ExpressionStatement {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.expression)
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Identifier {
    pub value: String,
}

impl Display for Identifier {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.value)
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct IntegerLiteral {
    pub token: token::Token,
    pub value: i64,
}

impl Display for IntegerLiteral {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.value)
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Boolean {
    pub value: bool,
}

impl Display for Boolean {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.value)
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct PrefixExpression {
    pub operator: String,
    pub right: Box<ExpressionNode>,
}

impl Display for PrefixExpression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "({}{})", self.operator, self.right)
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct InfixExpression {
    pub left: Box<ExpressionNode>,
    pub operator: String,
    pub right: Box<ExpressionNode>,
}

impl Display for InfixExpression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "({} {} {})", self.left, self.operator, self.right)
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct IfExpression {
    pub condition: Box<ExpressionNode>,
    pub consequence: BlockStatement,
    pub alternative: Option<BlockStatement>,
}

impl Display for IfExpression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if let Some(alt) = &self.alternative {
            write!(
                f,
                "if {} {{\n{}\n}} else {{\n{}\n}}",
                self.condition, self.consequence, alt
            )
        } else {
            write!(f, "if {} {{\n{}\n}}", self.condition, self.consequence)
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct BlockStatement {
    pub statements: Vec<StatementNode>,
}

impl Display for BlockStatement {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for stmt in &self.statements {
            _ = write!(f, "{}\n", stmt);
        }
        Ok(())
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FunctionLiteral {
    pub parameters: Vec<Identifier>,
    pub body: BlockStatement,
}

impl Display for FunctionLiteral {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let params = self
            .parameters
            .iter()
            .map(|p| p.to_string())
            .collect::<Vec<_>>()
            .join(", ");
        write!(f, "fn({}) {{\n{}\n}}", params, self.body)
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct CallExpression {
    pub function: Box<ExpressionNode>,
    pub arguments: Vec<ExpressionNode>,
}

impl Display for CallExpression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let args = self
            .arguments
            .iter()
            .map(|a| a.to_string())
            .collect::<Vec<_>>()
            .join(", ");
        write!(f, "{} ({})", self.function, args)
    }
}
