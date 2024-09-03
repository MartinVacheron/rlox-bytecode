use std::collections::HashMap;
use thiserror::Error;

use crate::chunk::{Chunk, Op};
use crate::compiler::ByteCodeGen;
use crate::debug::Disassembler;
use crate::gc::{Gc, GcFormatter, GcRef};
use crate::object::{BoundMethod, Closure, Instance, Iterator, Struct, UpValue};
use crate::value::{NativeFunction, Value};

use crate::native_fn::clock;

#[derive(Default)]
pub struct VmFlags {
    pub disassemble_compiled: bool,
    pub disassemble_instructions: bool,
    pub print_stack: bool,
}

#[derive(Error, Debug)]
pub enum VmErr {
    #[error("")]
    Compile,

    #[error("")]
    Runtime,
}

pub type VmRes = Result<(), VmErr>;

#[derive(Debug)]
struct CallFrame {
    closure: GcRef<Closure>,
    ip: usize,
    slots: usize,
}

pub struct Vm {
    flags: VmFlags,
    gc: Gc,
    stack: Vec<Value>,
    frames: Vec<CallFrame>,
    globals: HashMap<GcRef<String>, Value>,
    open_upvalues: Vec<GcRef<UpValue>>,
    init_string: GcRef<String>,
}

impl Vm {
    const FRAMES_MAX: usize = 64;
    const STACK_SIZE: usize = Self::FRAMES_MAX * u8::MAX as usize;

    pub fn new(flags: VmFlags) -> Self {
        let mut gc = Gc::default();
        let init_string = gc.intern("init".into());

        let mut vm = Self {
            flags,
            gc,
            stack: Vec::with_capacity(Self::STACK_SIZE),
            frames: Vec::with_capacity(Self::FRAMES_MAX),
            globals: HashMap::new(),
            open_upvalues: vec![],
            init_string,
        };

        vm.define_native("clock", clock);
        vm
    }

    pub fn interpret(&mut self, code: &str) -> VmRes {
        let bytecode_gen = ByteCodeGen::new(code, &mut self.gc, self.flags.disassemble_compiled);

        let function = match bytecode_gen.compile() {
            Ok(f) => f,
            Err(_) => return Err(VmErr::Compile),
        };

        self.push(Value::Fn(function));

        let closure = self.gc.alloc(Closure::from_fn(function));
        self.frames.push(CallFrame {
            closure,
            ip: 0,
            slots: 0,
        });

        self.run()
    }

    fn run(&mut self) -> VmRes {
        loop {
            if self.flags.disassemble_instructions {
                let disassembler = Disassembler::new(&self.gc, self.chunk(), Some(&self.stack));

                if self.flags.print_stack {
                    disassembler.print_stack();
                }

                disassembler.disassemble_instruction(self.frame().ip);
            }

            let op = self.chunk().code[self.frame().ip];
            self.frame_mut().ip += 1;

            match op {
                Op::Return => {
                    let res = self.pop();
                    let old = self.frames.pop().unwrap();

                    // Close all upvalues that must live longer than the function
                    self.close_upvalue(old.slots);

                    if self.frames.is_empty() {
                        // Fictive 'main' function
                        // self.pop();
                        return Ok(());
                    }

                    // Goes back to before fn call + args
                    self.stack.truncate(old.slots);
                    self.push(res);
                }
                Op::Constant(idx) => {
                    let val = self.chunk().constants[idx as usize].clone();
                    self.push(val);
                }
                Op::Negate => {
                    if let Err(e) = self.peek_mut(0).negate() {
                        self.runtime_err(&e.to_string())
                    }
                }
                Op::Add => {
                    let (rhs, lhs) = (self.pop(), self.pop());

                    let value = match (lhs, rhs) {
                        (Value::Int(v1), Value::Int(v2)) => Value::Int(v1 + v2),
                        (Value::Float(v1), Value::Float(v2)) => Value::Float(v1 + v2),
                        (Value::Str(v1), Value::Str(v2)) => {
                            let s1 = self.gc.deref(&v1);
                            let s2 = self.gc.deref(&v2);
                            let result = format!("{}{}", s1, s2);
                            let result = self.gc.intern(result);
                            Value::Str(result)
                        }
                        _ => {
                            dbg!(lhs, rhs);
                            self.runtime_err("Operation not allowed");
                            return Err(VmErr::Runtime);
                        }
                    };

                    self.push(value);
                }
                Op::Subtract => self.binop(|a, b| a.sub(b)),
                Op::Multiply => self.binop(|a, b| a.mul(b)),
                Op::Divide => self.binop(|a, b| a.div(b)),
                Op::True => self.push(Value::Bool(true)),
                Op::False => self.push(Value::Bool(false)),
                Op::Null => self.push(Value::Null),
                Op::Not => {
                    if let Err(e) = self.peek_mut(0).not() {
                        self.runtime_err(&e.to_string())
                    }
                }
                Op::Equal => self.binop(|a, b| a.eq(b)),
                Op::Greater => self.binop(|a, b| a.gt(b)),
                Op::Less => self.binop(|a, b| a.lt(b)),
                Op::Print => {
                    let value = self.pop();
                    println!("{}", GcFormatter::new(&value, &self.gc));
                }
                Op::Pop => {
                    self.pop();
                }
                Op::DefineGlobal(idx) => {
                    let name = self.chunk().read_string(idx);
                    let value = self.pop();
                    self.globals.insert(name, value);
                }
                Op::GetGlobal(idx) => {
                    let name = self.chunk().read_string(idx);

                    match self.globals.get(&name) {
                        Some(&glob) => self.push(glob),
                        None => {
                            let name = self.gc.deref(&name);
                            self.runtime_err(&format!("Undefined variable '{}'", name));
                            return Err(VmErr::Runtime);
                        }
                    }
                }
                Op::SetGlobal(idx) => {
                    let name = self.chunk().read_string(idx);

                    if self.globals.insert(name, self.peek(0)).is_none() {
                        self.runtime_err(&format!("Undefined variable '{}'", self.gc.deref(&name)));
                        return Err(VmErr::Runtime);
                    }
                }
                Op::GetLocal(idx) => {
                    let offset = self.frame().slots;
                    self.push(self.stack[idx as usize + offset]);
                }
                Op::SetLocal(idx) => {
                    let offset = self.frame().slots;
                    self.stack[idx as usize + offset] = self.peek(0);
                }
                Op::JumpIfFalse(idx) => {
                    if let Value::Bool(b) = self.peek(0) {
                        if !b {
                            self.frame_mut().ip += idx as usize;
                        }
                    }
                }
                Op::Jump(idx) => self.frame_mut().ip += idx as usize,
                Op::Loop(idx) => self.frame_mut().ip -= idx as usize,
                Op::CreateIter => {
                    if let Value::Int(i) = self.pop() {
                        // The placeholder value (same as local idx)
                        self.push(Value::Int(0));
                        let iter = Iterator { range: 0..i };
                        let iter = self.gc.alloc(iter);
                        self.push(Value::Iter(iter));
                    } else {
                        self.runtime_err("Range must be an integer");
                        return Err(VmErr::Runtime);
                    }
                }
                Op::ForIter(iter, idx) => {
                    let iter_idx = self.frame().slots + iter as usize;

                    if let Value::Iter(iter) = &self.stack[iter_idx] {
                        let iter = self.gc.deref_mut(iter);

                        match iter.range.next() {
                            Some(v) => {
                                if let Value::Int(i) = &mut self.stack[iter_idx - 1] {
                                    *i = v;
                                } else {
                                    self.runtime_err("failed to get next integer iterator value");
                                    return Err(VmErr::Runtime);
                                }
                            }
                            None => self.frame_mut().ip += idx as usize,
                        }
                    } else {
                        panic!("failed to find iterator")
                    }
                }
                Op::Call(args_count) => {
                    let callee = self.peek(args_count as usize);

                    if let Err(e) = self.call_value(callee, args_count) {
                        eprintln!("{}", e.to_string());
                        return Err(VmErr::Runtime);
                    }
                }
                Op::Closure(idx) => {
                    let function = self.chunk().read_constant(idx);

                    if let Value::Fn(function) = function {
                        let mut closure = Closure::from_fn(function);
                        let upvalue_count = self.gc.deref(&function).upvalues.len();

                        for i in 0..upvalue_count {
                            let upvalue = self.gc.deref(&function).upvalues[i];

                            let obj_upavlue = if upvalue.is_local {
                                self.capture_value(self.frame().slots + upvalue.index as usize)
                            } else {
                                self.closure().upvalues[upvalue.index as usize]
                            };

                            closure.upvalues.push(obj_upavlue);
                        }

                        let closure = self.gc.alloc(closure);
                        self.push(Value::Closure(closure));
                    } else {
                        panic!("closure instruction without function value")
                    }
                }
                Op::GetUpValue(idx) => {
                    let upvalue = self.closure().upvalues[idx as usize];
                    let upvalue = self.gc.deref(&upvalue);

                    let val = match upvalue.closed {
                        Some(v) => v,
                        None => self.stack[upvalue.location],
                    };

                    self.push(val);
                }
                Op::SetUpValue(idx) => {
                    let value = self.peek(0);
                    let upvalue = self.closure().upvalues[idx as usize];
                    let upvalue = self.gc.deref_mut(&upvalue);

                    if upvalue.closed.is_none() {
                        self.stack[upvalue.location] = value;
                    } else {
                        upvalue.closed = Some(value);
                    }
                }
                Op::CloseUpValue => {
                    self.close_upvalue(self.stack.len() - 1);
                    self.pop();
                }
                Op::Struct(idx) => {
                    let name = self.chunk().read_string(idx);
                    let structure = Struct::new(name);
                    let structure = self.gc.alloc(structure);
                    self.push(Value::Struct(structure));
                }
                // NOTE: Field shadowing methods, do we want that?
                Op::GetProperty(idx) => {
                    if let Value::Instance(inst) = self.peek(0) {
                        let inst = self.gc.deref(&inst);
                        let property_name = self.chunk().read_string(idx);
                        
                        match inst.fields.get(&property_name) {
                            Some(&value) => {
                                self.pop();
                                self.push(value);
                            }
                            None => self.bind_method(inst.parent, property_name)?,
                        }
                    } else {
                        self.runtime_err("only instances have field");
                        return Err(VmErr::Runtime);
                    };
                }
                Op::SetProperty(idx) => {
                    //foo.bar = 4 -> stack: foo, bar, 4
                    let value = self.pop();
                    let field_name = self.chunk().read_string(idx);

                    if let Value::Instance(inst) = self.pop() {
                        let inst = self.gc.deref_mut(&inst);

                        match inst.fields.insert(field_name, value) {
                            Some(_) => {}
                            None => eprintln!("Instance dosen't have field"),
                        }
                    }

                    // We put the assignement value back on top to return it as:
                    //  print toast.jam = "grape" // Prints "grape".
                    self.push(value);
                }
                Op::Method(idx) => {
                    let method_name = self.chunk().read_string(idx);

                    if let Err(e) = self.define_method(method_name) {
                        eprintln!("{}", e.to_string());
                        return Err(VmErr::Runtime);
                    }
                }
            }
        }
    }

    fn binop(&mut self, operation: fn(Value, Value) -> Option<Value>) {
        // Pop backward as it is a stack
        let (rhs, lhs) = (self.pop(), self.pop());

        match operation(lhs, rhs) {
            Some(res) => self.push(res),
            None => self.runtime_err("Operation not allowed"),
        }
    }

    fn call_value(&mut self, callee: Value, args_count: u8) -> VmRes {
        match callee {
            Value::Closure(f) => self.call(f, args_count)?,
            Value::NativeFn(f) => {
                let res = f(
                    args_count as usize,
                    (self.stack.len() - 1) - args_count as usize,
                );
                self.stack
                    .truncate(self.stack.len() - args_count as usize + 1);
                self.push(res);
            }
            Value::Struct(struct_ref) => {
                let instance = Instance::new(struct_ref);
                let instance = self.gc.alloc(instance);
                self.set_at(Value::Instance(instance), args_count);
                let parent = self.gc.deref(&struct_ref);

                if let Some(&Value::Closure(initializer)) = parent.methods.get(&self.init_string) {
                    self.call(initializer, args_count)?;
                } else if args_count != 0 {
                    self.runtime_err(&format!("expected 0 argument but got {}", args_count));
                    return Err(VmErr::Runtime);
                }
            }
            Value::BoundMethod(bound) => {
                let bound = self.gc.deref(&bound);
                let method = bound.method;
                let receiver = bound.receiver;
                self.set_at(receiver, args_count);
                self.call(method, args_count)?;
            }
            _ => {
                self.runtime_err("can only call functions and structures");
                return Err(VmErr::Runtime);
            }
        }

        Ok(())
    }

    fn call(&mut self, closure_ref: GcRef<Closure>, args_count: u8) -> VmRes {
        let closure = self.gc.deref(&closure_ref);
        let function = self.gc.deref(&closure.function);

        if self.frames.len() == Self::FRAMES_MAX {
            self.runtime_err("stack overflow");
            return Err(VmErr::Runtime);
        }

        if function.arity != args_count as usize {
            self.runtime_err(&format!(
                "expected {} arguments but got {}",
                function.arity, args_count
            ));

            return Err(VmErr::Runtime);
        }

        self.frames.push(CallFrame {
            closure: closure_ref,
            ip: 0,
            slots: self.stack.len() - args_count as usize - 1, // -1 to get the fn call
        });

        Ok(())
    }

    fn define_method(&mut self, name: GcRef<String>) -> VmRes {
        let method = self.pop();

        if let Value::Struct(structure) = self.peek(0) {
            let structure = self.gc.deref_mut(&structure);
            structure.methods.insert(name, method);

            Ok(())
        } else {
            self.runtime_err("can only call functions and structures");
            Err(VmErr::Runtime)
        }
    }

    fn bind_method(&mut self, parent: GcRef<Struct>, name: GcRef<String>) -> Result<(), VmErr> {
        let structure = self.gc.deref(&parent);
        if let Some(method) = structure.methods.get(&name) {
            let receiver = self.peek(0);

            let method = match method {
                Value::Closure(closure) => *closure,
                _ => panic!("inconsitent state, method is not a closure"),
            };
            let bound = BoundMethod::new(receiver, method);
            let bound = self.gc.alloc(bound);
            self.pop();
            self.push(Value::BoundMethod(bound));
            Ok(())
        } else {
            self.runtime_err(&format!(
                "instance dosen't have field {}",
                self.gc.deref(&name)
            ));
            Err(VmErr::Runtime)
        }
    }

    fn capture_value(&mut self, index: usize) -> GcRef<UpValue> {
        // Ensure only one UpValue pointing to a slot
        for upvalue_ref in self.open_upvalues.iter().rev() {
            let upvalue = self.gc.deref(upvalue_ref);

            if upvalue.location < index {
                break;
            }

            if upvalue.location == index {
                return *upvalue_ref;
            }
        }

        let upvalue = UpValue::new(index);
        let upvalue = self.gc.alloc(upvalue);
        self.open_upvalues.push(upvalue);
        upvalue
    }

    // Close all upvalues that point to slot above it in the stack
    fn close_upvalue(&mut self, last: usize) {
        let mut i = self.open_upvalues.len();

        loop {
            if i < 1 {
                break;
            }

            let upvalue_ref = self.open_upvalues[i - 1];
            let upvalue = self.gc.deref_mut(&upvalue_ref);

            if upvalue.location < last {
                break;
            }

            upvalue.closed = Some(self.stack[upvalue.location]);

            i -= 1;
        }

        self.open_upvalues.truncate(i);
    }

    fn define_native(&mut self, name: &str, function: NativeFunction) {
        let name = self.gc.intern(name.into());
        self.globals.insert(name, Value::NativeFn(function));
    }

    fn pop(&mut self) -> Value {
        self.stack.pop().unwrap()
    }

    fn push(&mut self, value: Value) {
        self.stack.push(value);
    }

    fn frame(&self) -> &CallFrame {
        self.frames.last().unwrap()
    }

    fn frame_mut(&mut self) -> &mut CallFrame {
        self.frames.last_mut().unwrap()
    }

    fn chunk(&self) -> &Chunk {
        &self.gc.deref(&self.closure().function).chunk
    }

    fn closure(&self) -> &Closure {
        self.gc.deref(&self.frame().closure)
    }

    fn peek(&self, distance: usize) -> Value {
        self.stack[self.stack.len() - distance - 1]
    }

    fn peek_mut(&mut self, distance: usize) -> &mut Value {
        let idx = self.stack.len() - distance - 1;
        &mut self.stack[idx]
    }

    fn set_at(&mut self, value: Value, offset: u8) {
        let index = self.stack.len() - offset as usize - 1;
        self.stack[index] = value;
    }

    fn runtime_err(&self, msg: &str) {
        eprintln!(
            "[line {}] Error: {}",
            self.chunk().lines[self.chunk().code.len() - 1] + 1,
            msg
        );

        for frame in self.frames.iter().rev() {
            let closure = self.gc.deref(&frame.closure);
            let function = self.gc.deref(&closure.function);
            let name = self.gc.deref(&function.name);

            eprint!("[line {}] in ", function.chunk.lines[frame.ip] + 1,);

            if name.is_empty() {
                eprintln!("script");
            } else {
                eprintln!("{}()", name);
            }
        }
    }
}
