use anyhow::{bail, Result};
use Value::*;

use crate::{gc::{Gc, GcRef, GcTrace}, object::{BoundMethod, Closure, Function, Instance, Iterator, Struct}};


#[derive(Clone, Copy, PartialEq, Debug)]
pub enum Value {
    Int(i64),
    Float(f64),
    Bool(bool),
    Str(GcRef<String>),
    Iter(GcRef<Iterator>),
    Fn(GcRef<Function>),
    Closure(GcRef<Closure>),
    NativeFn(NativeFunction),
    Struct(GcRef<Struct>),
    Instance(GcRef<Instance>),
    BoundMethod(GcRef<BoundMethod>),
    Null,
}

pub type NativeFunction = fn(usize, usize) -> Value;


impl Value {
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
            (Int(_), Int(_))
            | (Float(_), Float(_))
            | (Bool(_), Bool(_))
            | (Str(_), Str(_))
            | (Null, Null) => Some(Bool(self == other)),
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

impl GcTrace for Value {
    fn format<'gc>(&self, f: &mut std::fmt::Formatter, gc: &'gc crate::gc::Gc) -> std::fmt::Result {
        match self {
            Int(v) => write!(f, "{}", v),
            Float(v) => write!(f, "{}", v),
            Bool(v) => write!(f, "{}", v),
            Str(v) => gc.deref(v).format(f, gc),
            Iter(v) => gc.deref(v).format(f, gc),
            Null => write!(f, "null"),
            Fn(v) => gc.deref(v).format(f, gc),
            NativeFn(_) => write!(f, "<native fn>"),
            Closure(v) => gc.deref(v).format(f, gc),
            Struct(v) => gc.deref(v).format(f, gc),
            Instance(v) => gc.deref(v).format(f, gc),
            BoundMethod(v) => gc.deref(v).format(f, gc),
        }
    }

    fn trace(&self, gc: &mut Gc) {
        match self {
            Value::BoundMethod(v) => gc.mark_object(*v),
            Value::Struct(v) => gc.mark_object(*v),
            Value::Closure(v) => gc.mark_object(*v),
            Value::Fn(v) => gc.mark_object(*v),
            Value::Instance(v) => gc.mark_object(*v),
            Value::Str(v) => gc.mark_object(*v),
            _ => (),
        }
    }

    fn size(&self) -> usize {
        0
    }

    fn as_any(&self) -> &dyn std::any::Any {
        panic!("cannot dereference this value")
    }

    fn as_any_mut(&mut self) -> &mut dyn std::any::Any {
        panic!("cannot dereference this value")
    }
}