use std::time::{SystemTime, UNIX_EPOCH};

use crate::value::Value;


// arity: usize, stack_start: usize

pub fn clock(_: usize, _: usize) -> Value {
    match SystemTime::now().duration_since(UNIX_EPOCH) {
        Ok(t) => Value::Float(t.as_millis() as f64 / 1000.),
        Err(_) => panic!("Error getting time")
    }
}