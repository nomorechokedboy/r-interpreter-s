use std::fmt::Display;

#[derive(Clone, Debug)]
pub enum Object {
    Int64(i64),
    Bool(bool),
    Return(Box<Object>),
    Error(String),
    Null,
}

impl Object {
    pub fn inspect(&self) -> String {
        match self {
            Object::Int64(val) => format!("{val}"),
            Object::Bool(val) => format!("{val}"),
            Object::Null => "null".to_string(),
            Object::Return(val) => format!("{val}"),
            Object::Error(val) => format!("{val}"),
        }
    }
}

impl Display for Object {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Object::Int64(_) => write!(f, "INTEGER"),
            Object::Bool(_) => write!(f, "BOOLEAN"),
            Object::Null => write!(f, "NULL"),
            Object::Return(_) => write!(f, "RETURN_VALUE"),
            Object::Error(_) => write!(f, "ERROR"),
        }
    }
}
