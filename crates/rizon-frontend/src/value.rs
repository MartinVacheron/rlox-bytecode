use std::{cell::RefCell, fmt::Display, ops::Range, rc::Rc};
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
    Fn(Rc<Function>),
    ClosureFn(Closure),
    NativeFn(NativeFunction),
    Null,
}

#[derive(Debug)]
pub struct Function {
    pub arity: usize,
    pub name: String,
    pub chunk: Chunk,
    pub upvalues: Vec<FnUpValue>,
}

impl Function {
    pub fn add_upvalue(&mut self, index: u8, is_local: bool) {
        self.upvalues.push(FnUpValue { index, is_local });
    }
}

impl Default for Function {
    fn default() -> Self {
        Self {
            arity: 0,
            name: "".into(),
            chunk: Chunk::default(),
            upvalues: Vec::with_capacity(256),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct FnUpValue {
    pub index: u8,
    pub is_local: bool,
}

impl PartialEq for Function{
    fn eq(&self, other: &Self) -> bool {
        self.name == other.name
    }
}


#[derive(Clone, Debug, Default, PartialEq)]
pub struct Closure {
    pub function: Rc<Function>,
     // Multiple closure can point to the same upvalue (non local ones)
    pub upvalues: Vec<Rc<RefCell<UpValue>>>,
}

impl Closure {
    pub fn from_fn(function: &Rc<Function>) -> Self {
        Self {
            function: function.clone(),
            upvalues: Vec::with_capacity(function.upvalues.len()),
        }
    }
}

#[derive(Clone, PartialEq, Debug)]
pub struct UpValue {
    pub location: usize,
    pub closed: Option<Value>,
}

impl UpValue {
    pub fn new(location: usize) -> Self {
        Self { location, closed: None }
    }
}


pub type NativeFunction = fn(usize, usize) -> Value;


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
            (Null, Null) => Some(Bool(true)),
            (_, Null) => Some(Bool(false)),
            (Null, _) => Some(Bool(false)),
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

impl Value {
    pub fn new_closure(value: &Rc<Function>) -> Self {
        Self::ClosureFn(Closure::from_fn(value))
    }
}

impl From<Function> for Value {
    fn from(value: Function) -> Self {
        Self::Fn(Rc::new(value))
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
            Fn(v) => print_fn(v, f),
            NativeFn(_) => write!(f, "<native fn>"),
            ClosureFn(v) => print_fn(&v.function, f),
        }
    }
}

fn print_fn(function: &Rc<Function>, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    if !function.name.is_empty() {
        write!(f, "<fn {}>", function.name)
    } else {
        write!(f, "<script>")
    }
}