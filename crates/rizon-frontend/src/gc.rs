use std::{
    any::{type_name, Any}, collections::HashMap, fmt::{Debug, Display, Formatter}, hash::Hash, marker::PhantomData
};

use crate::object::ObjHeader;

pub trait GcTrace {
    fn format<'gc>(&self, f: &mut Formatter, gc: &'gc Gc) -> std::fmt::Result;
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

#[derive(Default)]
pub struct Gc {
    objects: Vec<ObjHeader>,
    strings: HashMap<String, GcRef<String>>,
}

impl Gc {
    pub fn alloc<T: GcTrace + 'static>(&mut self, obj: T) -> GcRef<T> {
        let header = ObjHeader {
            is_marked: false,
            obj: Box::new(obj),
        };

        self.objects.push(header);
        GcRef::new(self.objects.len() - 1)
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
            .obj
            .as_any()
            .downcast_ref()
            .unwrap_or_else(|| panic!("reference not found in GC"))
    }

    pub fn deref_mut<T: GcTrace + 'static>(&mut self, gc_ref: &GcRef<T>) -> &mut T {
        self.objects[gc_ref.index]
            .obj
            .as_any_mut()
            .downcast_mut()
            .unwrap_or_else(|| panic!("reference not found in GC"))
    }
}
