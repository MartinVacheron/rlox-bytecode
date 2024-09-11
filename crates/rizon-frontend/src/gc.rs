use std::fmt::Debug;
use std::hash::{Hash, Hasher};
use std::ops::{Deref, DerefMut};
// use std::collections::HashMap;
use std::ptr::NonNull;
use std::any::Any;

use ahash::AHashMap;

use crate::object::{BoundMethod, Closure, Function, Instance, Struct, UpValue, Iterator};
use crate::value::Value;


pub struct GcObject {
    pub marked: bool,
    pub data: NonNull<dyn Any>,  // Store the object as a trait object
    pub next: Option<NonNull<GcObject>>,
}

impl GcObject {
    pub unsafe fn new<T: Any>(data: T) -> Self {
        Self {
            marked: false,
            data: NonNull::new_unchecked(Box::into_raw(Box::new(data))),
            next: None,
        }
    }
}

pub struct GcRef<T> {
    pointer: NonNull<T>,
     // To access marked field
    // Alternative, see note on marked field
    gc_object: NonNull<GcObject>,
}

impl<T> GcRef<T> {
    pub fn dangling() -> Self {
        Self {
            pointer: NonNull::dangling(),
            gc_object: NonNull::dangling(),
        }
    }
}

impl<T> GcRef<T> {
    pub fn new(pointer: NonNull<T>, gc_object: NonNull<GcObject>) -> Self {
        Self { pointer, gc_object }
    }
}

impl<T> Deref for GcRef<T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        unsafe { self.pointer.as_ref() }
    }
}

impl<T> DerefMut for GcRef<T> {
    fn deref_mut(&mut self) -> &mut T {
        unsafe { self.pointer.as_mut() }
    }
}

impl<T> Copy for GcRef<T> {}

impl<T> Clone for GcRef<T> {
    fn clone(&self) -> GcRef<T> {
        *self
    }
}

impl<T> PartialEq for GcRef<T> {
    fn eq(&self, other: &Self) -> bool {
        self.pointer == other.pointer
    }
}

impl<T> Eq for GcRef<T> {}

impl Hash for GcRef<String> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.pointer.hash(state)
    }
}

impl<T: Debug> Debug for GcRef<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let inner: &T = unsafe { self.pointer.cast::<T>().as_ref() };
        write!(f, "{:?}", inner)
    }
}

pub struct Gc {
    first: Option<NonNull<GcObject>>,  // Now it's a NonNull<GcObject>
    grey_stack: Vec<NonNull<GcObject>>,
    strings: AHashMap<String, GcRef<String>>,
    bytes_allocated: usize,
    next_gc: usize,
    debug: bool,
}

impl Gc {
    const HEAP_GROW_FACTOR: usize = 2;

    pub fn new(debug: bool) -> Self {
        Self {
            first: None,
            grey_stack: Vec::new(),
            strings: AHashMap::new(),
            bytes_allocated: 0,
            next_gc: 1024 * 1024,
            debug,
        }
    }

    pub fn alloc<T: Any>(&mut self, obj: T) -> GcRef<T> {
        let size = std::mem::size_of::<T>() + std::mem::size_of::<GcObject>();
        self.bytes_allocated += size;

        // Create a new GcObject with type-erased data (Box<dyn Any>)
        let gc_object = unsafe { GcObject::new(obj) };

        // Allocate memory for the GcObject
        let mut pointer = unsafe { NonNull::new_unchecked(Box::into_raw(Box::new(gc_object))) };

        unsafe { pointer.as_mut().next = self.first.take() };
        self.first = Some(pointer);

        unsafe { GcRef { pointer: pointer.as_ref().data.cast(), gc_object: pointer } }
    }

    pub fn intern(&mut self, s: String) -> GcRef<String> {
        if let Some(&value) = self.strings.get(&s) {
            value
        } else {
            let reference = self.alloc(s.clone());
            self.strings.insert(s, reference);
            reference
        }
    }

    pub fn should_gc(&self) -> bool {
        // TODO feature
        self.bytes_allocated > self.next_gc
        // true
    }

    pub unsafe fn collect_garbage(&mut self) {
        // TODO: debug print
        let before = self.bytes_allocated;

        self.trace_references();
        self.remove_withe_strings();
        self.sweep();
        self.next_gc = self.bytes_allocated * Gc::HEAP_GROW_FACTOR;
    }

    unsafe fn trace_references(&mut self) {
        while let Some(pointer) = self.grey_stack.pop() {
            self.blacken_object(pointer);
        }
    }

    unsafe fn blacken_object(&mut self, pointer: NonNull<GcObject>) {
        match pointer.as_ref().data.as_ref() {
            obj if obj.is::<String>() => {},
            obj if obj.is::<Function>() => {
                let function = pointer.as_ref().data.cast::<Function>().as_ref();
                self.mark_object(&function.name);

                for &constant in &function.chunk.constants {
                    self.mark_value(&constant);
                }
            },
            obj if obj.is::<Closure>() => {
                let closure = pointer.as_ref().data.cast::<Closure>().as_ref();
                self.mark_object(&closure.function);

                for &upvalue in &closure.upvalues {
                    self.mark_object(&upvalue);
                }
            },
            obj if obj.is::<UpValue>() => {
                let upvalue = pointer.as_ref().data.cast::<UpValue>().as_ref();
                if let Some(closed) = upvalue.closed {
                    self.mark_value(&closed);
                }
            },
            obj if obj.is::<Struct>() => {
                let structure = pointer.as_ref().data.cast::<Struct>().as_ref();
                self.mark_object(&structure.name);
                self.mark_table(&structure.methods);
            },
            obj if obj.is::<Instance>() => {
                let instance = pointer.as_ref().data.cast::<Instance>().as_ref();
                self.mark_object(&instance.structure);
                self.mark_table(&instance.fields);
            },
            obj if obj.is::<BoundMethod>() => {
                let bound = pointer.as_ref().data.cast::<BoundMethod>().as_ref();
                self.mark_value(&bound.receiver);
                self.mark_object(&bound.method);
            },
            obj if obj.is::<Iterator>() => {
                todo!()
            },
            _ => panic!("blacken illegal object")
        }
    }

    pub fn mark_value(&mut self, value: &Value) {
        match value {
            Value::Str(v) => self.mark_object(v),
            Value::Iter(v) => self.mark_object(v),
            Value::Fn(v) => self.mark_object(v),
            Value::Closure(v) => self.mark_object(v),
            Value::Struct(v) => self.mark_object(v),
            Value::Instance(v) => self.mark_object(v),
            Value::BoundMethod(v) => self.mark_object(v),
            _ => ()
        }
    }
    pub fn mark_object<T: Any>(&mut self, obj: &GcRef<T>) {
        self.grey_stack.push(obj.gc_object);
        unsafe { (*obj.gc_object.as_ptr()).marked = true }

        // TODO: configure a debug print
    }

    pub fn mark_table(&mut self, table: &AHashMap<GcRef<String>, Value>) {
        for (k, v) in table {
            self.mark_object(k);
            self.mark_value(v);
        }
    }

    fn sweep(&mut self) {
        let mut previous: Option<NonNull<GcObject>> = None;
        let mut current = self.first;

        while let Some(mut obj) = current {
            unsafe {
                let pointer = obj.as_mut();
                current = pointer.next;

                if pointer.marked {
                    pointer.marked = false;
                    previous = Some(obj);
                } else {
                    if let Some(mut prev) = previous {
                        prev.as_mut().next = pointer.next;
                    } else {
                        self.first = pointer.next;
                    }

                    self.bytes_allocated -= std::mem::size_of_val(pointer);

                    // TODO: debug print
                    _ = Box::from_raw(pointer);
                }
            }
        }
    }

    fn remove_withe_strings(&mut self) {
        unsafe {
            self.strings.retain(|_, v| (*v.gc_object.as_ptr()).marked)
        }
    }
}