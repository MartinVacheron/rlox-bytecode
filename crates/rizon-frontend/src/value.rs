use std::fmt::Display;

#[derive(Clone, PartialEq)]
pub enum Value {
    Float(f64),
    Bool(bool),
    Str(Box<String>),
    Null,
}


impl Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Float(v) => write!(f, "{}", v),
            Value::Bool(v) => write!(f, "{}", v),
            Value::Str(v) => write!(f, "\"{}\"", v),
            Value::Null => write!(f, "null"),
        }
    }
}
