use std::{fmt::Display, ops::Range};
use anyhow::{bail, Result};

use Value::*;

use crate::chunk::Chunk;

#[derive(Clone, PartialEq, Debug)]
pub enum Value {
    Int(i64),
    Float(f64),
    Bool(bool),
    Str(Box<String>),
    Iter(Range<i64>),
    Fn(Box<Function>),
    Null,
}

#[derive(Clone, Debug, Default)]
pub struct Function {
    pub arity: usize,
    pub name: String,
    pub chunk: Chunk,
}

impl PartialEq for Function {
    fn eq(&self, other: &Self) -> bool {
        self.name == other.name
    }
}

impl Value {
    pub fn add(self, other: Self) -> Option<Self> {
        match (self, other) {
            (Int(v1), Int(v2)) => Some(Int(v1 + v2)),
            (Float(v1), Float(v2)) => Some(Float(v1 + v2)),
            (Str(v1), Str(v2)) => Some(Str(Box::new(String::from(*v1 + &*v2)))),
            _ => None,
        }
    }

    pub fn sub(self, other: Self) -> Option<Self> {
        match (self, other) {
            (Int(v1), Int(v2)) => Some(Int(v1 - v2)),
            (Float(v1), Float(v2)) => Some(Float(v1 - v2)),
            _ => None,
        }
    }

    pub fn mul(self, other: Self) -> Option<Self> {
        match (self, other) {
            (Int(v1), Int(v2)) => Some(Int(v1 * v2)),
            (Float(v1), Float(v2)) => Some(Float(v1 * v2)),
            _ => None,
        }
    }

    pub fn div(self, other: Self) -> Option<Self> {
        match (self, other) {
            (Int(v1), Int(v2)) => Some(Int(v1 / v2)),
            (Float(v1), Float(v2)) => Some(Float(v1 / v2)),
            _ => None,
        }
    }

    pub fn eq(self, other: Self) -> Option<Self> {
        match (self, other) {
            (Int(v1), Int(v2)) => Some(Bool(v1 == v2)),
            (Float(v1), Float(v2)) => Some(Bool(v1 == v2)),
            (Bool(v1), Bool(v2)) => Some(Bool(v1 == v2)),
            (Str(v1), Str(v2)) => Some(Bool(v1 == v2)),
            _ => None,
        }
    }

    pub fn lt(self, other: Self) -> Option<Self> {
        match (self, other) {
            (Int(v1), Int(v2)) => Some(Bool(v1 < v2)),
            (Float(v1), Float(v2)) => Some(Bool(v1 < v2)),
            _ => None,
        }
    }

    pub fn gt(self, other: Self) -> Option<Self> {
        match (self, other) {
            (Int(v1), Int(v2)) => Some(Bool(v1 > v2)),
            (Float(v1), Float(v2)) => Some(Bool(v1 > v2)),
            _ => None,
        }
    }

    pub fn negate(&mut self) -> Result<()> {
        match self {
            Int(v) => *v *= -1,
            Float(v) => *v *= -1.,
            _ => bail!("can't negate type other than int and float")
        }

        Ok(())
    }

    pub fn not(&mut self) -> Result<()> {
        match self {
            Bool(v) => *v = !*v,
            _ => bail!("can't use not operator on other type than bool")
        }

        Ok(())
    }
}


impl Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Int(v) => write!(f, "{}", v),
            Float(v) => write!(f, "{}", v),
            Bool(v) => write!(f, "{}", v),
            Str(v) => write!(f, "\"{}\"", v),
            Iter(v) => write!(f, "range {} -> {}", v.start, v.end),
            Null => write!(f, "null"),
            Fn(v) => {
                if !v.name.is_empty() {
                    write!(f, "<fn {}>", v.name)
                } else {
                    write!(f, "<script>")
                }
            }
        }
    }
}
