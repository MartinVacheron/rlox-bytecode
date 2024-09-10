use std::{collections::HashMap, ops::Range};

use ahash::AHashMap;

use crate::{
    chunk::Chunk,
    value::Value,
    gc::GcRef,
};

// Iterators
#[derive(Debug)]
pub struct Iterator {
    pub range: Range<i64>,
}



// Function
#[derive(Debug)]
pub struct Function {
    pub arity: usize,
    pub name: GcRef<String>,
    pub chunk: Chunk,
    pub upvalues: Vec<FnUpValue>,
}

impl Function {
    pub fn add_upvalue(&mut self, index: u8, is_local: bool) {
        self.upvalues.push(FnUpValue { index, is_local });
    }
}

impl Function {
    pub fn new(name: GcRef<String>) -> Self {
        Self {
            arity: 0,
            name,
            chunk: Chunk::default(),
            upvalues: Vec::with_capacity(256),
        }
    }
}


#[derive(Clone, Copy, Debug)]
pub struct FnUpValue {
    pub index: u8,
    pub is_local: bool,
}


#[derive(Debug)]
pub struct Closure {
    pub function: GcRef<Function>,
    // Multiple closure can point to the same upvalue (non local ones)
    pub upvalues: Vec<GcRef<UpValue>>,
}


impl Closure {
    pub fn from_fn(function: GcRef<Function>) -> Self {
        Self {
            function,
            upvalues: Vec::new(),
        }
    }
}


#[derive(Debug)]
pub struct UpValue {
    pub location: usize,
    pub closed: Option<Value>,
}

impl UpValue {
    pub fn new(location: usize) -> Self {
        Self {
            location,
            closed: None,
        }
    }
}


#[derive(Debug)]
pub struct Struct {
    pub name: GcRef<String>,
    pub methods: AHashMap<GcRef<String>, Value>,
}

impl Struct {
    pub fn new(name: GcRef<String>) -> Self {
        Self {
            name,
            methods: AHashMap::new(),
        }
    }
}


#[derive(Debug)]
pub struct Instance {
    pub structure: GcRef<Struct>,
    pub fields: AHashMap<GcRef<String>, Value>,
}

impl Instance {
    pub fn new(structure: GcRef<Struct>) -> Self {
        Self {
            structure,
            fields: AHashMap::new(),
        }
    }
}


#[derive(Debug)]
pub struct BoundMethod {
    pub receiver: Value,
    pub method: GcRef<Closure>,
}

impl BoundMethod {
    pub fn new(receiver: Value, method: GcRef<Closure>) -> Self {
        Self { receiver, method }
    }
}
