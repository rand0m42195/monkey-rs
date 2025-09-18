use std::cell::RefCell;
use std::collections::HashMap;
use std::fmt::Display;
use std::rc::Rc;

use crate::ast;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Object {
    Null,
    Integer(i64),
    String(String),
    Boolean(bool),
    Return(Box<Object>),
    Function(
        Vec<ast::Identifier>,
        ast::BlockStatement,
        Rc<RefCell<Environment>>,
    ),
}

impl Display for Object {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Object::Null => write!(f, "null"),
            Object::Integer(n) => write!(f, "{}", n),
            Object::String(s) => write!(f, "{}", s),
            Object::Boolean(b) => write!(f, "{}", b),
            Object::Return(r) => write!(f, "{}", r),
            Object::Function(params, block, _env) => {
                let params = params
                    .iter()
                    .map(|p| p.to_string())
                    .collect::<Vec<_>>()
                    .join(", ");
                write!(f, "fn({}) {{\n{}}}", params, block)
            }
        }
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum ObjectType {
    Null,
    Integer,
    String,
    Boolean,
    Return,
    Function,
}

impl Object {
    pub fn type_of(&self) -> ObjectType {
        match self {
            Object::Null => ObjectType::Null,
            Object::Integer(_) => ObjectType::Integer,
            Object::String(_) => ObjectType::String,
            Object::Boolean(_) => ObjectType::Boolean,
            Object::Return(_) => ObjectType::Return,
            Self::Function(_, _, _) => ObjectType::Function,
        }
    }

    pub fn is_type(&self, ty: ObjectType) -> bool {
        self.type_of() == ty
    }
}

impl Display for ObjectType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ObjectType::Null => write!(f, "Null"),
            ObjectType::Integer => write!(f, "Integer"),
            ObjectType::String => write!(f, "String"),
            ObjectType::Boolean => write!(f, "Boolean"),
            ObjectType::Return => write!(f, "Return"),
            ObjectType::Function => write!(f, "Function"),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
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
