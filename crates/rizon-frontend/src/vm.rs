use std::collections::HashMap;

use thiserror::Error;

use crate::chunk::{Chunk, Op};
use crate::compiler::Compiler;
use crate::debug::disassemble_instruction;
use crate::value::Value;

#[derive(Default)]
pub struct VmFlags {
    pub disassemble_compiled: bool,
    pub disassemble_instructions: bool,
}

#[derive(Error, Debug)]
pub enum VmErr {
    #[error("")]
    Compile,

    #[error("")]
    Runtime,
}

pub type VmRes = Result<Value, VmErr>;

#[derive(Default)]
pub struct Vm {
    chunk: Chunk,
    ip: usize,
    stack: Vec<Value>,
    flags: VmFlags,
    globals: HashMap<String, Value>,
}

impl Vm {
    pub fn new(flags: VmFlags) -> Self {
        Self {
            flags,
            ..Default::default()
        }
    }

    pub fn interpret(&mut self, code: &str) -> VmRes {
        let mut compiler = Compiler::new(code, &mut self.chunk);

        compiler.compile(self.flags.disassemble_compiled);

        self.run()
    }

    fn run(&mut self) -> VmRes {
        loop {
            if self.flags.disassemble_instructions {
                print!("          ");
                self.stack.iter().for_each(|v| print!("[{}] ", v));
                println!();

                disassemble_instruction(&self.chunk, self.ip);
            }

            let op = self.eat().clone();

            match op {
                Op::Return => return Ok(Value::Float(0.)),
                Op::Constant(idx) => {
                    let val = self.chunk.constants[idx as usize].clone();
                    self.push(val);
                }
                Op::Negate => {
                    let mut val = self.pop();

                    if let Value::Float(f) = &mut val {
                        *f *= -1.;
                    } else {
                        self.runtime_err("Can't negate anything than a number")
                    }

                    self.push(val);
                }
                Op::Add => {
                    let (rhs, lhs) = (self.pop(), self.pop());

                    match (lhs, rhs) {
                        (Value::Float(v1), Value::Float(v2)) => self.push(Value::Float(v1 + v2)),
                        (Value::Str(v1), Value::Str(v2)) => {
                            self.push(Value::Str(Box::new(String::from(*v1 + &*v2))))
                        }
                        _ => self.runtime_err("Operands must be numbers"),
                    }
                }
                Op::Subtract => self.binop(|a, b| a - b),
                Op::Multiply => self.binop(|a, b| a * b),
                Op::Divide => self.binop(|a, b| a / b),
                Op::True => self.push(Value::Bool(true)),
                Op::False => self.push(Value::Bool(false)),
                Op::Null => self.push(Value::Null),
                Op::Not => {
                    let mut val = self.pop();

                    if let Value::Bool(b) = &mut val {
                        *b = !*b;
                    } else {
                        self.runtime_err("Can't use 'not' operator on anything else than a bool")
                    }

                    self.push(val);
                }
                Op::Equal => {
                    let (v1, v2) = (self.pop(), self.pop());

                    self.push(Value::Bool(v1 == v2));
                }
                Op::Greater => self.binop_to_bool(|a, b| a > b),
                Op::Less => self.binop_to_bool(|a, b| a < b),
                Op::Print => println!("{}", self.pop()),
                Op::Pop => {
                    self.pop();
                }
                Op::DefineGlobal(idx) => match &self.chunk.constants[idx as usize] {
                    Value::Str(s) => {
                        let name = *s.clone();
                        let value = self.pop();
                        self.globals.insert(name, value);
                    }
                    _ => panic!("Internal error, using non-string operand to OP_DEFINE_GLOBAL"),
                },
                Op::GetGlobal(idx) => match &self.chunk.constants[idx as usize] {
                    Value::Str(s) => match self.globals.get(s.as_ref()) {
                        Some(glob) => self.push(glob.clone()),
                        None => {
                            self.runtime_err(&format!("Undefined variable '{}'", s));
                            return Err(VmErr::Runtime);
                        }
                    },
                    _ => panic!("Internal error, using non-string operand to OP_DEFINE_GLOBAL"),
                },
                Op::SetGlobal(idx) => match &self.chunk.constants[idx as usize] {
                    Value::Str(s) => {
                        let name = *s.clone();

                        if self.globals.insert(name, self.peek(0).clone()).is_none() {
                            self.runtime_err(&format!("Undefined variable '{}'", s));
                            return Err(VmErr::Runtime);
                        }
                    }
                    _ => panic!("Internal error, using non-string operand to OP_DEFINE_GLOBAL"),
                },
                Op::GetLocal(idx) => {
                    self.push(self.stack[idx as usize].clone());
                }
                Op::SetLocal(idx) => {
                    self.stack[idx as usize] = self.peek(0).clone();
                },
                Op::JumpIfFalse(idx) => {
                    if let Value::Bool(b) = self.peek(0) {
                        if !b {
                            self.ip += idx as usize;
                        }
                    }
                },
                Op::Jump(idx) => self.ip += idx as usize,
                Op::Loop(idx) => {
                    dbg!(idx, self.ip);
                    self.ip -= idx as usize;
                }
            }
        }
    }

    fn binop(&mut self, operation: fn(f64, f64) -> f64) {
        // Pop backward as it is a stack
        let (rhs, lhs) = (self.pop(), self.pop());

        match (lhs, rhs) {
            (Value::Float(f1), Value::Float(f2)) => self.push(Value::Float(operation(f1, f2))),
            _ => self.runtime_err("Operands must be numbers"),
        }
    }

    fn binop_to_bool(&mut self, operation: fn(f64, f64) -> bool) {
        // Pop backward as it is a stack
        let (rhs, lhs) = (self.pop(), self.pop());

        match (lhs, rhs) {
            (Value::Float(f1), Value::Float(f2)) => self.push(Value::Bool(operation(f1, f2))),
            _ => self.runtime_err("Operands must be numbers"),
        }
    }

    fn eat(&mut self) -> &Op {
        let op = &self.chunk.code[self.ip];
        self.ip += 1;

        op
    }

    fn pop(&mut self) -> Value {
        self.stack.pop().unwrap()
    }

    fn push(&mut self, value: Value) {
        self.stack.push(value);
    }

    fn peek(&self, distance: usize) -> &Value {
        &self.stack[self.stack.len() - distance - 1]
    }

    fn runtime_err(&self, msg: &str) {
        eprintln!(
            "[line {}] Error: {}",
            self.chunk.lines[self.chunk.code.len() - 1],
            msg
        );
    }
}
