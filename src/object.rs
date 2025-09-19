use std::cell::RefCell;
use std::collections::HashMap;
use std::fmt::Display;

use std::rc::Rc;

use crate::ast;
use crate::errors::MonkeyError;

pub type BuiltinFunc = fn(args: &mut [Object]) -> Result<Object, MonkeyError>;

#[derive(Debug, Clone)]
pub enum Object {
    Null,
    Integer(i64),
    String(String),
    Boolean(bool),
    Array(Vec<Object>),
    Hash(Vec<(Object, Object)>),
    Return(Box<Object>),
    Function(
        Vec<ast::Identifier>,
        ast::BlockStatement,
        Rc<RefCell<Environment>>,
    ),
    BuiltinFunction(BuiltinFunc),
}

impl Display for Object {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Null => write!(f, "null"),
            Self::Integer(n) => write!(f, "{}", n),
            Self::String(s) => write!(f, "{}", s),
            Self::Boolean(b) => write!(f, "{}", b),
            Self::Array(a) => {
                let s = a
                    .iter()
                    .map(|e| e.to_string())
                    .collect::<Vec<String>>()
                    .join(", ");
                write!(f, "[{}]", s)
            }
            Self::Hash(h) => {
                let s = h
                    .iter()
                    .map(|(k, v)| format!("{}: {}", k, v))
                    .collect::<Vec<String>>()
                    .join(", ");

                write!(f, "{{{}}}", s)
            }
            Self::Return(r) => write!(f, "{}", r),
            Self::Function(params, block, _env) => {
                let params = params
                    .iter()
                    .map(|p| p.to_string())
                    .collect::<Vec<_>>()
                    .join(", ");
                write!(f, "fn({}) {{\n{}}}", params, block)
            }
            // TODO:
            Self::BuiltinFunction(_b) => write!(f, "[builtin function]",),
        }
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum ObjectType {
    Null,
    Integer,
    String,
    Boolean,
    Array,
    Hash,
    Return,
    Function,
    BuiltinFunction,
}

impl Object {
    pub fn type_of(&self) -> ObjectType {
        match self {
            Self::Null => ObjectType::Null,
            Self::Integer(_) => ObjectType::Integer,
            Self::String(_) => ObjectType::String,
            Self::Boolean(_) => ObjectType::Boolean,
            Self::Array(_) => ObjectType::Array,
            Self::Hash(_) => ObjectType::Hash,
            Self::Return(_) => ObjectType::Return,
            Self::Function(_, _, _) => ObjectType::Function,
            Self::BuiltinFunction(_) => ObjectType::BuiltinFunction,
        }
    }

    pub fn is_type(&self, ty: ObjectType) -> bool {
        self.type_of() == ty
    }
}

impl Display for ObjectType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Null => write!(f, "Null"),
            Self::Integer => write!(f, "Integer"),
            Self::String => write!(f, "String"),
            Self::Boolean => write!(f, "Boolean"),
            Self::Array => write!(f, "Array"),
            Self::Hash => write!(f, "Hash"),
            Self::Return => write!(f, "Return"),
            Self::Function => write!(f, "Function"),
            Self::BuiltinFunction => write!(f, "BuiltinFunction"),
        }
    }
}

#[derive(Debug, Clone)]
pub struct Environment {
    store: HashMap<ast::Identifier, Object>,
    outer: Option<Box<Environment>>,
}

impl Environment {
    pub fn new(outer: Option<Box<Environment>>) -> Environment {
        Environment {
            store: HashMap::new(),
            outer,
        }
    }
    pub fn get(&self, identifier: &ast::Identifier) -> Option<Object> {
        if let Some(obj) = self.store.get(identifier) {
            Some(obj.clone())
        } else {
            if let Some(outer) = &self.outer {
                outer.get(identifier)
            } else {
                None
            }
        }
    }

    pub fn set(&mut self, key: &ast::Identifier, val: &Object) {
        self.store.insert(key.clone(), val.clone());
    }
}
