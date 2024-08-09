use std::fmt::Display;

#[derive(Clone, Debug)]
pub enum Object {
    Int64(i64),
    Bool(bool),
    Null,
}

impl Object {
    pub fn inspect(&self) -> String {
        match self {
            Object::Int64(val) => format!("{val}"),
            Object::Bool(val) => format!("{val}"),
            Object::Null => "null".to_string(),
        }
    }
}

impl Display for Object {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Object::Int64(_) => write!(f, "INTEGER"),
            Object::Bool(_) => write!(f, "BOOL"),
            Object::Null => write!(f, "NULL"),
        }
    }
}
