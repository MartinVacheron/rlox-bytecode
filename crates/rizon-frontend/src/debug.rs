use std::ops::Deref;

use crate::{chunk::{Chunk, Op}, object::FnUpValue, value::Value};


pub struct Disassembler<'vm> {
    chunk: &'vm Chunk,
    stack: Option<&'vm Vec<Value>>,
}

impl<'vm> Disassembler<'vm> {
    pub fn new(chunk: &'vm Chunk, stack: Option<&'vm Vec<Value>>) -> Self {
        Self {
            chunk,
            stack,
        }
    }

    pub fn print_stack(&self) {
        if let Some(stack) = self.stack {
            print!("          ");
            stack.iter().for_each(|v| print!("[ {} ] ", v));
            println!();
        }
    }

    pub fn disassemble(&self, name: &str) {
        println!("\t\t== {} ==", name);

        for idx in 0..self.chunk.code.len() {
            self.disassemble_instruction(idx);
        }
    }

    pub fn disassemble_instruction(&self, offset: usize) {
        print!("{:04} ", offset);
    
        if offset > 0 && self.chunk.lines[offset - 1] == self.chunk.lines[offset] {
            print!("   | ");
        } else {
            print!("{:4} ", self.chunk.lines[offset] + 1);
        }
    
        match &self.chunk.code[offset] {
            Op::Return => println!("OP_RETURN"),
            Op::Constant(i) => self.constant("OP_CONSTANT", *i),
            Op::Negate => println!("OP_NEGATE"),
            Op::Add => println!("OP_ADD"),
            Op::Subtract => println!("OP_SUBTRACT"),
            Op::Multiply => println!("OP_MULTIPLY"),
            Op::Divide => println!("OP_DIVIDE"),
            Op::Null => println!("OP_NULL"),
            Op::True => println!("OP_TRUE"),
            Op::False => println!("OP_FALSE"),
            Op::Not => println!("OP_NOT"),
            Op::Equal => println!("OP_EQUAL"),
            Op::Greater => println!("OP_GREATER"),
            Op::Less => println!("OP_LESS"),
            Op::Print => println!("OP_PRINT"),
            Op::Pop => println!("OP_POP"),
            Op::DefineGlobal(i) => self.constant("OP_DEFINE_GLOBAL", *i),
            Op::GetGlobal(i) => self.constant("OP_GET_GLOBAL", *i),
            Op::SetGlobal(i) => self.constant("OP_SET_GLOBAL", *i),
            Op::GetLocal(i) => byte_instruction("OP_GET_LOCAL", *i),
            Op::SetLocal(i) => byte_instruction("OP_SET_LOCAL", *i),
            Op::JumpIfFalse(i) => jump_instruction("JUMP_IF_FALSE", offset, *i),
            Op::Jump(i) => jump_instruction("OP_JUMP", offset, *i),
            Op::Loop(i) => loop_instruction("OP_LOOP", offset, *i),
            Op::CreateIter => println!("OP_CREATE_ITER"),
            Op::ForIter(i, j) => self.for_iter_instruction(*i, offset, *j),
            Op::Call(i) => byte_instruction("OP_CALL", *i),
            Op::Closure(i) => self.closure_instruction(offset, *i),
            Op::GetUpValue(i) => byte_instruction("OP_GET_UPVALUE", *i),
            Op::SetUpValue(i) => byte_instruction("OP_SET_UPVALUE", *i),
            Op::CloseUpValue => println!("OP_CLOSE_VALUE"),
            Op::Struct(i) => self.constant("OP_STRUCT", *i),
            Op::GetProperty(i) => self.constant("OP_GET_PROPERTY", *i),
            Op::SetProperty(i) => self.constant("OP_SET_PROPERTY", *i),
            Op::Method(i) => self.constant("OP_METHOD", *i),
            Op::Invoke((i, j)) => self.invoke_instruction(*i, *j),
        }
    }

    fn constant(&self, name: &str, idx: u8) {
        let constant = &self.chunk.constants[idx as usize];
        println!("{:16} {} '{}'", name, idx, constant);
    }

    fn closure_instruction(&self, offset: usize, idx: u8) {
        let closure = &self.chunk.constants[idx as usize];
        println!("{:16} {} '{}'", "OP_CLOSURE", idx, closure);

        if let Value::Closure(closure) = closure {
            // let closure = self.gc.deref(closure);
            let function = closure.function.deref();

            for (i, FnUpValue { index, is_local }) in function.upvalues.iter().enumerate() {
                let local = if *is_local { "local" } else { "upvalue" };
                println!("{:04}      |                     {} {}", offset + i, local, index);
            }
        }
    }

    fn for_iter_instruction(&self, iter_slot: u8, start_jump: usize, end_jump: u16) {
        println!(
            "{:16} slot {}, loop {} -> {}",
            "OP_FOR_ITER",
            iter_slot,
            start_jump,
            end_jump,
        );
    }

    fn invoke_instruction(&self, name_idx: u8, args_count: u8) {
        let name = self.chunk.constants[name_idx as usize];

        println!(
            "{:16} ({} args) {}",
            "OP_INVOKE",
            args_count,
            name,
        );
    }
}


fn byte_instruction(name: &str, slot: u8) {
    println!("{:16} {}", name, slot);
}

fn jump_instruction(name: &str, start: usize, offset: u16) {
    println!("{:16} {} -> {}", name, start, start as i32 + offset as i32 + 1);
}

// +1 is ok, it's because the Op::Loop is taken into account
fn loop_instruction(name: &str, start: usize, offset: u16) {
    println!("{:16} {} -> {}", name, start, start as i32 + 1 + offset as i32 * -1);
}

