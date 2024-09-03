use std::{collections::HashMap, mem, ops::Range};

use crate::{
    chunk::{Chunk, Op},
    gc::{Gc, GcRef, GcTrace},
    value::Value,
};

pub struct ObjHeader {
    pub is_marked: bool,
    pub size: usize,
    pub obj: Box<dyn GcTrace>,
}

// String
impl GcTrace for String {
    fn format<'gc>(&self, f: &mut std::fmt::Formatter, _: &'gc Gc) -> std::fmt::Result {
        write!(f, "\"{}\"", self)
    }

    fn trace(&self, _: &mut Gc) {}

    fn size(&self) -> usize {
        mem::size_of::<String>() + self.capacity()
    }

    fn as_any(&self) -> &dyn std::any::Any {
        self
    }

    fn as_any_mut(&mut self) -> &mut dyn std::any::Any {
        self
    }
}


// Iterators
#[derive(Debug)]
pub struct Iterator {
    pub range: Range<i64>,
}

impl GcTrace for Iterator {
    fn format<'gc>(&self, f: &mut std::fmt::Formatter, _: &'gc Gc) -> std::fmt::Result {
        write!(f, "range {} -> {}", self.range.start, self.range.end)
    }

    fn trace(&self, _: &mut Gc) {
        todo!()
    }

    fn size(&self) -> usize {
        mem::size_of::<std::ops::Range<i64>>()
    }

    fn as_any(&self) -> &dyn std::any::Any {
        self
    }

    fn as_any_mut(&mut self) -> &mut dyn std::any::Any {
        self
    }
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

impl GcTrace for Function {
    fn format(&self, f: &mut std::fmt::Formatter, gc: &Gc) -> std::fmt::Result {
        let name = gc.deref(&self.name);

        if name.is_empty() {
            write!(f, "<script>")
        } else {
            write!(f, "<fn {}>", name)
        }
    }

    fn trace(&self, gc: &mut Gc) {
        gc.mark_object(self.name);
        
        for &constant in &self.chunk.constants {
            gc.mark_value(constant);
        }
    }

    fn size(&self) -> usize {
        mem::size_of::<Function>()
            + self.upvalues.capacity() * mem::size_of::<FnUpValue>()
            + self.chunk.code.capacity() * mem::size_of::<Op>()
            + self.chunk.lines.capacity() * mem::size_of::<usize>()
            + self.chunk.constants.capacity() * mem::size_of::<Value>()
    }

    fn as_any(&self) -> &dyn std::any::Any {
        self
    }

    fn as_any_mut(&mut self) -> &mut dyn std::any::Any {
        self
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

impl GcTrace for Closure {
    fn format(&self, f: &mut std::fmt::Formatter, gc: &Gc) -> std::fmt::Result {
        gc.deref(&self.function).format(f, gc)
    }

    fn trace(&self, gc: &mut Gc) {
        gc.mark_object(self.function);

        for &upvalue in &self.upvalues {
            gc.mark_object(upvalue);
        }
    }

    fn size(&self) -> usize {
        mem::size_of::<Closure>() + self.upvalues.capacity() * mem::size_of::<GcRef<UpValue>>()
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

impl GcTrace for UpValue {
    fn format<'gc>(&self, f: &mut std::fmt::Formatter, _: &'gc Gc) -> std::fmt::Result {
        write!(f, "upvalue")
    }

    fn trace(&self, gc: &mut Gc) {
        if let Some(obj) = self.closed {
            gc.mark_value(obj)
        }
    }

    fn size(&self) -> usize {
        mem::size_of::<UpValue>()
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
    fn format(&self, f: &mut std::fmt::Formatter, gc: &Gc) -> std::fmt::Result {
        let name = gc.deref(&self.name);
        write!(f, "<struct {}>", name)
    }

    fn trace(&self, gc: &mut Gc) {
        gc.mark_object(self.name);
        gc.mark_table(&self.methods);
    }

    fn size(&self) -> usize {
        mem::size_of::<Struct>()
    }

    fn as_any(&self) -> &dyn std::any::Any {
        self
    }

    fn as_any_mut(&mut self) -> &mut dyn std::any::Any {
        self
    }
}


#[derive(Debug)]
pub struct Instance {
    pub structure: GcRef<Struct>,
    pub fields: HashMap<GcRef<String>, Value>,
}

impl Instance {
    pub fn new(structure: GcRef<Struct>) -> Self {
        Self {
            structure,
            fields: HashMap::new(),
        }
    }
}

impl GcTrace for Instance {
    fn format(&self, f: &mut std::fmt::Formatter, gc: &Gc) -> std::fmt::Result {
        let parent = gc.deref(&self.structure);
        let name = gc.deref(&parent.name);
        write!(f, "<instance of {}>", name)
    }

    fn trace(&self, gc: &mut Gc) {
        gc.mark_object(self.structure);
        gc.mark_table(&self.fields);
    }

    fn size(&self) -> usize {
        mem::size_of::<Instance>()
    }

    fn as_any(&self) -> &dyn std::any::Any {
        self
    }

    fn as_any_mut(&mut self) -> &mut dyn std::any::Any {
        self
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

impl GcTrace for BoundMethod {
    fn format(&self, f: &mut std::fmt::Formatter, gc: &Gc) -> std::fmt::Result {
        gc.deref(&self.method).format(f, gc)
    }

    fn trace(&self, gc: &mut Gc) {
        gc.mark_value(self.receiver);
        gc.mark_object(self.method);
    }

    fn size(&self) -> usize {
        mem::size_of::<BoundMethod>()
    }

    fn as_any(&self) -> &dyn std::any::Any {
        self
    }

    fn as_any_mut(&mut self) -> &mut dyn std::any::Any {
        self
    }
}
