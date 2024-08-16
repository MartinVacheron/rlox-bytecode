use crate::chunk::{Chunk, Op};

pub fn disassemble(chunk: &Chunk, name: &str) {
    println!("\t\t== {} ==", name);

    for idx in 0..chunk.code.len() {
        disassemble_instruction(chunk, idx);
    }
}

pub fn disassemble_instruction(chunk: &Chunk, offset: usize) {
    print!("{:04} ", offset);

    if offset > 0 && chunk.lines[offset - 1] == chunk.lines[offset] {
        print!("   | ");
    } else {
        print!("{:4} ", chunk.lines[offset] + 1);
    }

    match &chunk.code[offset] {
        Op::Return => println!("OP_RETURN"),
        Op::Constant(i) => constant("OP_CONSTANT", chunk, *i),
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
        Op::DefineGlobal(i) => constant("OP_DEFINE_GLOBAL", chunk, *i),
        Op::GetGlobal(i) => constant("OP_GET_GLOBAL", chunk, *i),
        Op::SetGlobal(i) => constant("OP_SET_GLOBAL", chunk, *i),
        Op::GetLocal(i) => byte_instruction("OP_GET_LOCAL", *i),
        Op::SetLocal(i) => byte_instruction("OP_SET_LOCAL", *i),
        Op::JumpIfFalse(i) => jump_instruction("JUMP_IF_FALSE", offset as i32, *i, 1),
        Op::Jump(i) => jump_instruction("OP_JUMP", offset as i32, *i, 1),
        Op::Loop(i) => jump_instruction("OP_LOOP", offset as i32, *i, -1),
        Op::CreateIter => println!("OP_CREATE_ITER"),
        Op::ForIter(i) => jump_instruction("OP_FOR_ITER", offset as i32, *i, 1),
        Op::Call(i) => byte_instruction("OP_CALL", *i),
        Op::Closure(i) => constant("OP_CLOSURE", chunk, *i),
    }
}

fn constant(name: &str, chunk: &Chunk, idx: u8) {
    println!("{:16} {} '{}'", name, idx, chunk.constants[idx as usize]);
}

fn byte_instruction(name: &str, slot: u8) {
    println!("{:16} {}", name, slot);
}

fn jump_instruction(name: &str, start: i32, offset: u16, sign: i32) {
    println!("{:16} {} -> {}", name, start, 1 + start + offset as i32 * sign);
}
