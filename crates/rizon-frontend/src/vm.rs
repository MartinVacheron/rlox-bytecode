use thiserror::Error;

use crate::chunk::{Chunk, Op};
use crate::compiler::Compiler;
use crate::debug::disassemble_instruction;
use crate::value::Value;

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

pub struct Vm {
    chunk: Chunk,
    ip: usize,
    stack: Vec<Value>,
    flags: VmFlags,
}

impl Vm {
    pub fn new(flags: VmFlags) -> Self {
        Self {
            chunk: Chunk::default(),
            ip: 0,
            stack: vec![],
            flags,
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

            let tk = self.eat().clone();

            match tk {
                Op::Return => {
                    let val = self.pop();
                    println!("{}", val);
                    return Ok(val);
                }
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
                Op::Add => self.binop(|a, b| a + b),
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
                        self.runtime_err("Can't use 'not' operate on anything else than a bool")
                    }

                    self.push(val);
                }
                Op::Equal => {
                    let v1 = self.pop();
                    let v2 = self.pop();

                    self.push(Value::Bool(v1 == v2));
                }
                Op::Greater => self.binop_to_bool(|a, b| a > b),
                Op::Less => self.binop_to_bool(|a, b| a < b),
            }
        }
    }

    fn binop(&mut self, operation: fn(f64, f64) -> f64) {
        // Pop backward as it is a stack
        let (rhs, lhs) = (self.pop(), self.pop());

        match (lhs, rhs) {
            (Value::Float(f1), Value::Float(f2)) => {
                self.push(Value::Float(operation(f1, f2)))
            }
            _ => self.runtime_err("Operands must be numbers"),
        }
    }

    fn binop_to_bool(&mut self, operation: fn(f64, f64) -> bool) {
        // Pop backward as it is a stack
        let (rhs, lhs) = (self.pop(), self.pop());

        match (lhs, rhs) {
            (Value::Float(f1), Value::Float(f2)) => {
                self.push(Value::Bool(operation(f1, f2)))
            }
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
