// use num_enum::{IntoPrimitive, TryFromPrimitive};
use crate::value::Value;

// #[derive(IntoPrimitive, TryFromPrimitive)]
#[derive(Clone)]
#[repr(u8)]
pub enum Op {
    Return,
    Constant(u8),
    Negate,
    Add,
    Subtract,
    Multiply,
    Divide,
    Null,
    True,
    False,
    Not,
    Equal, // TODO: implement the three others: https://craftinginterpreters.com/types-of-values.html#equality-and-comparison-operators
    Greater,
    Less,
    Print,
    Pop,
    DefineGlobal(u8),
    GetGlobal(u8),
    SetGlobal(u8),
}

#[derive(Default)]
pub struct Chunk {
    // code: Vec<u8>
    pub code: Vec<Op>,
    pub constants: Vec<Value>,
    pub lines: Vec<usize>,
}

impl Chunk {
    pub fn write(&mut self, byte: Op, line: usize) {
        self.code.push(byte);
        self.lines.push(line);
    }

    pub fn write_constant(&mut self, value: Value) -> usize {
        self.constants.push(value);

        self.constants.len() - 1
    }
}
