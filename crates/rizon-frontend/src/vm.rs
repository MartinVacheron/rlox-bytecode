use std::any::Any;
use std::collections::HashMap;
use std::fmt::Debug;
use std::ops::{Deref, DerefMut};
use thiserror::Error;

use crate::chunk::{Chunk, Op};
use crate::compiler::ByteCodeGen;
use crate::debug::Disassembler;
use crate::gc::{Gc, GcRef};
use crate::object::{BoundMethod, Closure, Instance, Iterator, Struct, UpValue};
use crate::value::{NativeFunction, Value};

use crate::native_fn::clock;

#[derive(Default)]
pub struct VmFlags {
    pub disassemble_compiled: bool,
    pub disassemble_instructions: bool,
    pub print_stack: bool,
    pub verbose_gc: bool,
    pub stress_gc: bool,
}

#[derive(Error, Debug)]
pub enum VmErr {
    #[error("")]
    Compile,

    #[error("")]
    Runtime,
}

pub type VmRes = Result<(), VmErr>;

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
        let mut gc = Gc::new(flags.verbose_gc);
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

        let closure = self.alloc(Closure::from_fn(function));
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
                let disassembler = Disassembler::new(self.chunk(), Some(&self.stack));

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
                    let val = self.chunk().constants[idx as usize];
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
                            let result = format!("{}{}", v1.deref(), v2.deref());
                            let result = self.intern(result);
                            Value::Str(result)
                        }
                        _ => {
                            self.runtime_err("Operation not allowed");
                            return Err(VmErr::Runtime);
                        }
                    };

                    self.push(value);
                }
                Op::Subtract => self.binop(|a, b| a.sub(b))?,
                Op::Multiply => self.binop(|a, b| a.mul(b))?,
                Op::Divide => self.binop(|a, b| a.div(b))?,
                Op::True => self.push(Value::Bool(true)),
                Op::False => self.push(Value::Bool(false)),
                Op::Null => self.push(Value::Null),
                Op::Not => {
                    if let Err(e) = self.peek_mut(0).not() {
                        self.runtime_err(&e.to_string());
                        return Err(VmErr::Runtime)
                    }
                }
                Op::Equal => self.binop(|a, b| a.eq(b))?,
                Op::Greater => self.binop(|a, b| a.gt(b))?,
                Op::Less => self.binop(|a, b| a.lt(b))?,
                Op::Print => println!("{}", self.pop()),
                Op::Pop => { _ = self.pop(); },
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
                            self.runtime_err(&format!("Undefined variable '{}'", name.deref()));
                            return Err(VmErr::Runtime);
                        }
                    }
                }
                Op::SetGlobal(idx) => {
                    let name = self.chunk().read_string(idx);

                    if self.globals.insert(name, self.peek(0)).is_none() {
                        self.runtime_err(&format!("Undefined variable '{}'", name.deref()));
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
                    } else if let Value::Null = self.peek(0) {
                        self.frame_mut().ip += idx as usize;
                    }
                }
                Op::Jump(idx) => self.frame_mut().ip += idx as usize,
                Op::Loop(idx) => self.frame_mut().ip -= idx as usize,
                Op::CreateIter => {
                    if let Value::Int(i) = self.pop() {
                        // The placeholder value (same as local idx)
                        self.push(Value::Int(0));
                        let iter = Iterator { range: 0..i };
                        let iter = self.alloc(iter);
                        self.push(Value::Iter(iter));
                    } else {
                        self.runtime_err("Range must be an integer");
                        return Err(VmErr::Runtime);
                    }
                }
                Op::ForIter(iter, idx) => {
                    let iter_idx = self.frame().slots + iter as usize;

                    if let Value::Iter(mut iter) = &self.stack[iter_idx] {

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
                        println!("{}", e.to_string());
                        return Err(VmErr::Runtime);
                    }
                }
                Op::Closure(idx) => {
                    let function = self.chunk().read_constant(idx);

                    if let Value::Fn(function) = function {
                        let mut closure = Closure::from_fn(function);
                        let upvalue_count = function.upvalues.len();

                        for i in 0..upvalue_count {
                            let upvalue = function.upvalues[i];

                            let obj_upavlue = if upvalue.is_local {
                                self.capture_value(self.frame().slots + upvalue.index as usize)
                            } else {
                                self.closure().upvalues[upvalue.index as usize]
                            };

                            closure.upvalues.push(obj_upavlue);
                        }

                        let closure = self.alloc(closure);
                        self.push(Value::Closure(closure));
                    } else {
                        panic!("closure instruction without function value")
                    }
                }
                Op::GetUpValue(idx) => {
                    let upvalue = self.closure().upvalues[idx as usize];

                    let val = match upvalue.closed {
                        Some(v) => v,
                        None => self.stack[upvalue.location],
                    };

                    self.push(val);
                }
                Op::SetUpValue(idx) => {
                    let value = self.peek(0);
                    let mut upvalue = self.closure().upvalues[idx as usize];

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
                    let structure = self.alloc(structure);
                    self.push(Value::Struct(structure));
                }
                // NOTE: Field shadowing methods, do we want that?
                Op::GetProperty(idx) => {
                    if let Value::Instance(inst) = self.peek(0) {
                        let property_name = self.chunk().read_string(idx);

                        match inst.fields.get(&property_name) {
                            Some(&value) => {
                                self.pop();
                                self.push(value);
                            }
                            None => self.bind_method(inst.structure, property_name)?,
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

                    if let Value::Instance(mut inst) = self.pop() {
                        inst.fields.insert(field_name, value);
                    } else {
                        self.runtime_err("only instances have field");
                        return Err(VmErr::Runtime);
                    }

                    // We put the assignement value back on top to return it as:
                    //  print toast.jam = "grape" // Prints "grape".
                    self.push(value);
                }
                Op::Method(idx) => {
                    let method_name = self.chunk().read_string(idx);

                    if let Err(e) = self.define_method(method_name) {
                        println!("{}", e.to_string());
                        return Err(VmErr::Runtime);
                    }
                }
                Op::Invoke((name_idx, args_count)) => {
                    let method = self.chunk().read_string(name_idx);
                    self.invoke(method, args_count)?;
                }
            }
        }
    }

    fn binop(&mut self, operation: fn(Value, Value) -> Option<Value>) -> VmRes {
        // Pop backward as it is a stack
        let (rhs, lhs) = (self.pop(), self.pop());

        match operation(lhs, rhs) {
            Some(res) => {
                self.push(res);
                Ok(())
            },
            None => {
                self.runtime_err("Operation not allowed");
                Err(VmErr::Runtime)
            }
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
                    .truncate(self.stack.len() - args_count as usize - 1);
                self.push(res);
            }
            Value::Struct(struct_ref) => {
                let instance = Instance::new(struct_ref);
                let instance = self.alloc(instance);
                self.set_at(Value::Instance(instance), args_count);

                // PERF: do not look up a hashmap?
                // https://craftinginterpreters.com/methods-and-initializers.html#challenges
                if let Some(&Value::Closure(initializer)) = struct_ref.methods.get(&self.init_string)
                {
                    self.call(initializer, args_count)?;
                } else if args_count != 0 {
                    self.runtime_err(&format!("expected 0 argument but got {}", args_count));
                    return Err(VmErr::Runtime);
                }
            }
            Value::BoundMethod(bound) => {
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
        let function = closure_ref.function;

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

        if let Value::Struct(mut structure) = self.peek(0) {
            structure.methods.insert(name, method);

            Ok(())
        } else {
            self.runtime_err("can only call functions and structures");
            Err(VmErr::Runtime)
        }
    }

    fn bind_method(&mut self, structure: GcRef<Struct>, name: GcRef<String>) -> Result<(), VmErr> {
        if let Some(method) = structure.methods.get(&name) {
            let receiver = self.peek(0);

            let method = match method {
                Value::Closure(closure) => *closure,
                _ => panic!("inconsitent state, method is not a closure"),
            };
            let bound = BoundMethod::new(receiver, method);
            let bound = self.alloc(bound);
            self.pop();
            self.push(Value::BoundMethod(bound));
            Ok(())
        } else {
            self.runtime_err(&format!(
                "instance dosen't have field {}",
                name.deref()
            ));
            Err(VmErr::Runtime)
        }
    }

    fn capture_value(&mut self, index: usize) -> GcRef<UpValue> {
        // Ensure only one UpValue pointing to a slot
        for upvalue in self.open_upvalues.iter().rev() {
            if upvalue.location < index {
                break;
            }

            if upvalue.location == index {
                return *upvalue;
            }
        }

        let upvalue = UpValue::new(index);
        let upvalue = self.alloc(upvalue);
        self.open_upvalues.push(upvalue);
        upvalue
    }

    fn invoke(&mut self, method_name: GcRef<String>, args_count: u8) -> VmRes {
        let receiver = if let Value::Instance(inst) = self.peek(args_count as usize) {
            inst
        } else {
            self.runtime_err("only instances have methods");
            return Err(VmErr::Runtime);
        };

        // We first check if this is a field
        match receiver.fields.get(&method_name) {
            Some(&value) => {
                self.set_at(value, args_count);
                self.call_value(value, args_count)
            }
            None => self.invoke_from_struct(receiver.structure, &method_name, args_count)
        }
    }

    fn invoke_from_struct(
        &mut self,
        structure: GcRef<Struct>,
        method_name: &GcRef<String>,
        args_count: u8,
    ) -> VmRes {
        let method = if let Some(&Value::Closure(closure)) = structure.methods.get(method_name) {
            closure
        } else {
            self.runtime_err(&format!("undefined property '{}'", method_name.deref()));
            return Err(VmErr::Runtime);
        };

        self.call(method, args_count)
    }

    // Close all upvalues that point to slot above it in the stack
    fn close_upvalue(&mut self, last: usize) {
        let mut i = self.open_upvalues.len();

        loop {
            if i < 1 {
                break;
            }

            let mut upvalue = self.open_upvalues[i - 1];

            if upvalue.location < last {
                break;
            }

            upvalue.closed = Some(self.stack[upvalue.location]);

            i -= 1;
        }

        self.open_upvalues.truncate(i);
    }

    fn define_native(&mut self, name: &str, function: NativeFunction) {
        let name = self.intern(name.into());
        self.globals.insert(name, Value::NativeFn(function));
    }

    fn alloc<T: Any>(&mut self, object: T) -> GcRef<T> {
        self.mark_and_sweep();
        self.gc.alloc(object)
    }

    fn intern(&mut self, name: String) -> GcRef<String> {
        self.mark_and_sweep();
        self.gc.intern(name)
    }

    fn mark_and_sweep(&mut self) {
        if self.flags.stress_gc || self.gc.should_gc() {
            if self.flags.verbose_gc {
                println!("-- GC begin");
            }

            self.mark_roots();
            unsafe { self.gc.collect_garbage() };

            if self.flags.verbose_gc {
                println!("-- GC end");
            }
        }
    }

    fn mark_roots(&mut self) {
        for value in &self.stack {
            self.gc.mark_value(value);
        }

        for frame in &self.frames {
            self.gc.mark_object(&frame.closure)
        }

        for upvalue in &self.open_upvalues {
            self.gc.mark_object(upvalue);
        }

        self.gc.mark_table(&self.globals);
        self.gc.mark_object(&self.init_string);
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
        &self.closure().function.chunk
    }

    fn closure(&self) -> &Closure {
        self.frame().closure.deref()
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
        println!(
            "[line {}] Error: {}",
            self.chunk().lines[self.chunk().code.len() - 1] + 1,
            msg
        );

        for frame in self.frames.iter().rev() {
            let function = frame.closure.function;
            let name = function.name.deref();

            print!("[line {}] in ", function.chunk.lines[frame.ip] + 1,);

            if name.is_empty() {
                println!("script");
            } else {
                println!("{}()", name);
            }
        }
    }
}
