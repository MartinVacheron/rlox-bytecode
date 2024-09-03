use std::{collections::HashMap, ops::Range};

use crate::{chunk::Chunk, gc::{Gc, GcRef, GcTrace}, value::Value};


pub struct ObjHeader {
    pub is_marked: bool,
    pub obj: Box<dyn GcTrace>,
}


// String
impl GcTrace for String {
    fn format<'gc>(&self, f: &mut std::fmt::Formatter, _: &'gc Gc) -> std::fmt::Result {
        write!(f, "\"{}\"", self)
    }
    
    fn as_any(&self) -> &dyn std::any::Any {
        self
    }
    
    fn as_any_mut(&mut self) -> &mut dyn std::any::Any {
        self
    }
}


// Iterators
pub struct Iterator {
    pub range: Range<i64>,
}

impl GcTrace for Iterator {
    fn format<'gc>(&self, f: &mut std::fmt::Formatter, _: &'gc Gc) -> std::fmt::Result {
        write!(f, "range {} -> {}", self.range.start, self.range.end)
    }
    
    fn as_any(&self) -> &dyn std::any::Any {
        self
    }
    
    fn as_any_mut(&mut self) -> &mut dyn std::any::Any {
        self
    }
}


// Function
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

impl GcTrace for Function {
    fn format<'gc>(&self, f: &mut std::fmt::Formatter, gc: &'gc Gc) -> std::fmt::Result {
        let name = gc.deref(&self.name);

        if name.is_empty() {
            write!(f, "<script>")
        } else {
            write!(f, "<fn {}>", name)
        }
    }
    
    fn as_any(&self) -> &dyn std::any::Any {
        self
    }
    
    fn as_any_mut(&mut self) -> &mut dyn std::any::Any {
        self
    }
}


#[derive(Clone, Copy)]
pub struct FnUpValue {
    pub index: u8,
    pub is_local: bool,
}


pub struct Closure {
    pub function: GcRef<Function>,
     // Multiple closure can point to the same upvalue (non local ones)
    pub upvalues: Vec<GcRef<UpValue>>,
}

impl GcTrace for Closure {
    fn format<'gc>(&self, f: &mut std::fmt::Formatter, gc: &'gc Gc) -> std::fmt::Result {
        gc.deref(&self.function).format(f, gc)
    }
    
    fn as_any(&self) -> &dyn std::any::Any {
        self
    }
    
    fn as_any_mut(&mut self) -> &mut dyn std::any::Any {
        self
    }
}


impl Closure {
    pub fn from_fn(function: GcRef<Function>) -> Self {
        Self {
            function,
            upvalues: Vec::new(),
        }
    }
}



pub struct UpValue {
    pub location: usize,
    pub closed: Option<Value>,
}

impl UpValue {
    pub fn new(location: usize) -> Self {
        Self { location, closed: None }
    }
}

impl GcTrace for UpValue {
    fn format<'gc>(&self, f: &mut std::fmt::Formatter, _: &'gc Gc) -> std::fmt::Result {
        write!(f, "upvalue")
    }

    fn as_any(&self) -> &dyn std::any::Any {
        self
    }

    fn as_any_mut(&mut self) -> &mut dyn std::any::Any {
        self
    }
}


#[derive(Debug)]
pub struct Struct {
    pub name: GcRef<String>,
    pub methods: HashMap<GcRef<String>, Value>,
}

impl Struct {
    pub fn new(name: GcRef<String>) -> Self {
        Self {
            name: name,
            methods: HashMap::new(),
        }
    }
}

impl GcTrace for Struct {
    fn format<'gc>(&self, f: &mut std::fmt::Formatter, gc: &'gc Gc) -> std::fmt::Result {
        let name = gc.deref(&self.name);
        write!(f, "<struct {}>", name)
    }

    fn as_any(&self) -> &dyn std::any::Any {
        self
    }

    fn as_any_mut(&mut self) -> &mut dyn std::any::Any {
        self
    }
}



pub struct Instance {
    pub parent: GcRef<Struct>,
    pub fields: HashMap<GcRef<String>, Value>,
}

impl Instance {
    pub fn new(parent: GcRef<Struct>) -> Self {
        Self {
            parent,
            fields: HashMap::new(),
        }
    }
}

impl GcTrace for Instance {
    fn format<'gc>(&self, f: &mut std::fmt::Formatter, gc: &'gc Gc) -> std::fmt::Result {
        let parent = gc.deref(&self.parent);
        let name = gc.deref(&parent.name);
        write!(f, "<instance of {}>", name)
    }

    fn as_any(&self) -> &dyn std::any::Any {
        self
    }

    fn as_any_mut(&mut self) -> &mut dyn std::any::Any {
        self
    }
}



pub struct BoundMethod {
    pub receiver: Value,
    pub method: GcRef<Closure>,
}

impl BoundMethod {
    pub fn new(receiver: Value, method: GcRef<Closure>) -> Self {
        Self {
            receiver,
            method,
        }
    }
}

impl GcTrace for BoundMethod {
    fn format<'gc>(&self, f: &mut std::fmt::Formatter, gc: &'gc Gc) -> std::fmt::Result {
        gc.deref(&self.method).format(f, gc)
    }

    fn as_any(&self) -> &dyn std::any::Any {
        self
    }

    fn as_any_mut(&mut self) -> &mut dyn std::any::Any {
        self
    }
}