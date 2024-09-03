use std::{
    any::{type_name, Any}, collections::{HashMap, VecDeque}, fmt::{Debug, Display, Formatter}, hash::Hash, marker::PhantomData, mem
};

use crate::{object::ObjHeader, value::Value};

pub trait GcTrace {
    fn format(&self, f: &mut Formatter, gc: &Gc) -> std::fmt::Result;
    fn trace(&self, gc: &mut Gc);
    fn size(&self) -> usize;
    fn as_any(&self) -> &dyn Any;
    fn as_any_mut(&mut self) -> &mut dyn Any;
}

pub struct GcRef<T: GcTrace> {
    pub index: usize,
    _marker: PhantomData<T>,
}

impl<T: GcTrace> GcRef<T> {
    pub fn new(index: usize) -> Self {
        Self {
            index,
            _marker: PhantomData,
        }
    }
}

impl<T: GcTrace> Clone for GcRef<T> {
    #[inline]
    fn clone(&self) -> GcRef<T> {
        *self
    }
}

impl<T: GcTrace> Copy for GcRef<T> {}
impl<T: GcTrace> Eq for GcRef<T> {}

impl<T: GcTrace> Debug for GcRef<T> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let name = type_name::<T>();
        write!(f, "ref {}: {}", self.index, name)
    }
}

impl<T: GcTrace> PartialEq for GcRef<T> {
    fn eq(&self, other: &Self) -> bool {
        self.index == other.index
    }
}

impl<T: GcTrace> Hash for GcRef<T> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.index.hash(state)
    }
}



pub struct GcFormatter<'gc, T: GcTrace> {
    object: &'gc T,
    gc: &'gc Gc,
}

impl<'gc, T: GcTrace> GcFormatter<'gc, T> {
    pub fn new(object: &'gc T, gc: &'gc Gc) -> Self {
        Self { object, gc }
    }
}

impl<'gc, T: GcTrace> Display for GcFormatter<'gc, T> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        self.object.format(f, self.gc)
    }
}


pub struct Gc {
    objects: Vec<Option<ObjHeader>>,
    tombstones: Vec<usize>,
    grey_stack: VecDeque<usize>,
    strings: HashMap<String, GcRef<String>>,
    bytes_allocated: usize,
    next_gc: usize,
    debug: bool,
}

impl Gc {
    const HEAP_GROW_FACTOR: usize = 2;

    pub fn new(debug: bool) -> Self {
        Self {
            objects: vec![],
            tombstones: vec![],
            grey_stack: VecDeque::new(),
            strings: HashMap::new(),
            bytes_allocated: 0,
            next_gc: 1024 * 1024,
            debug,
        }
    }

    pub fn alloc<T: GcTrace + 'static + Debug>(&mut self, obj: T) -> GcRef<T> {
        let repr: String = if self.debug {
            format!("{:?}", obj).chars().take(32).collect()
        } else { "".into() };

        let size = obj.size() + mem::size_of::<ObjHeader>();
        let header = ObjHeader {
            is_marked: false,
            size,
            obj: Box::new(obj),
        };
        self.bytes_allocated += size;

        let index = match self.tombstones.pop() {
            Some(idx) => {
                self.objects[idx] = Some(header);
                idx
            }
            None => {
                self.objects.push(Some(header));
                self.objects.len() - 1
            }
        };

        if self.debug {
            println!(
                "alloc(id: {}, type: {}, repr: {}, bytes: {}, next: {})",
                index,
                type_name::<T>(),
                repr,
                self.bytes_allocated,
                self.next_gc,
            )
        }

        GcRef::new(index)
    }

    pub fn intern(&mut self, name: String) -> GcRef<String> {
        if let Some(&value) = self.strings.get(&name) {
            value
        } else {
            let reference = self.alloc(name.clone());
            self.strings.insert(name, reference);
            reference
        }
    }

    pub fn deref<T: GcTrace + 'static>(&self, gc_ref: &GcRef<T>) -> &T {
        self.objects[gc_ref.index]
            .as_ref()
            .unwrap()
            .obj
            .as_any()
            .downcast_ref()
            .unwrap_or_else(|| panic!("reference not found in GC"))
    }

    pub fn deref_mut<T: GcTrace + 'static>(&mut self, gc_ref: &GcRef<T>) -> &mut T {
        self.objects[gc_ref.index]
            .as_mut()
            .unwrap()
            .obj
            .as_any_mut()
            .downcast_mut()
            .unwrap_or_else(|| panic!("reference not found in GC"))
    }

    pub fn collect_garbage(&mut self) {
        let before = self.bytes_allocated;

        self.trace_references();
        self.remove_white_strings();
        self.sweep();
        self.next_gc = self.bytes_allocated * Gc::HEAP_GROW_FACTOR;

        if self.debug {
            println!(
                "collected {} bytes (from {} to {}) next at {}",
                before - self.bytes_allocated,
                before,
                self.bytes_allocated,
                self.next_gc
            );
        }
    }

    fn trace_references(&mut self) {
        while let Some(index) = self.grey_stack.pop_back() {
            self.blacken_object(index);
        }
    }

    fn blacken_object(&mut self, index: usize) {
        if self.debug {
            println!("blacken(id:{})", index);
        }

        // Hack to trick the borrow checker to be able to call trace on an element.
        let object = self.objects[index].take();
        object.as_ref().unwrap().obj.trace(self);
        self.objects[index] = object;
    }

    fn free(&mut self, index: usize) {
        if self.debug {
            println!("Freeing slot: {}", index);
        }

        if let Some(old) = self.objects[index].take() {       
            self.bytes_allocated -= old.size;
            self.tombstones.push(index)
        } else {
            panic!("Double free on {}", index)
        }
    }

    pub fn mark_value(&mut self, value: Value) {
        value.trace(self);
    }

    pub fn mark_object<T: GcTrace>(&mut self, obj: GcRef<T>) {
        if let Some(object) = self.objects[obj.index].as_mut() {
            if object.is_marked {
                return;
            }
    
            if self.debug {
                println!(
                    "mark(id:{}, type:{}, val:{:?})",
                    obj.index,
                    type_name::<T>(),
                    obj
                );
            }

            object.is_marked = true;
            self.grey_stack.push_back(obj.index);
        } else {
            panic!("Marking already freed object {}", obj.index)
        }
    }

    pub fn mark_table(&mut self, table: &HashMap<GcRef<String>, Value>) {
        for (&k, &v) in table {
            self.mark_object(k);
            self.mark_value(v);
        }
    }

    pub fn should_gc(&self) -> bool {
        self.bytes_allocated > self.next_gc
    }

    fn sweep(&mut self) {
        for i in 0..self.objects.len() {
            if let Some(object) = self.objects[i].as_mut() {
                if object.is_marked {
                    object.is_marked = false;
                } else {
                    self.free(i);
                }
            }
        }
    }

    fn remove_white_strings(&mut self) {
        let strings = &mut self.strings;
        let objects = &self.objects;
        strings.retain(|_k, v| objects[v.index].as_ref().unwrap().is_marked);
    }
}
