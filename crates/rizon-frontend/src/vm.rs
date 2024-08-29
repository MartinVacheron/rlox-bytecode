use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

use thiserror::Error;

use crate::chunk::Op;
use crate::compiler::ByteCodeGen;
use crate::debug::disassemble_instruction;
use crate::value::{Closure, FnUpValue, Instance, NativeFunction, Struct, UpValue, Value};

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
    closure: Closure,
    ip: usize,
    slots: usize,
}

#[derive(Default)]
pub struct Vm {
    flags: VmFlags,
    stack: Vec<Value>,
    frames: Vec<CallFrame>,
    globals: HashMap<String, Value>,
    open_upvalues: Vec<Rc<RefCell<UpValue>>>,
}

impl Vm {
    const FRAMES_MAX: usize = 64;
    const STACK_SIZE: usize = Self::FRAMES_MAX * u8::MAX as usize;

    pub fn new(flags: VmFlags) -> Self {
        let mut vm = Self {
            flags,
            stack: Vec::with_capacity(Self::STACK_SIZE),
            frames: Vec::with_capacity(Self::FRAMES_MAX),
            ..Default::default()
        };

        vm.define_native("clock", clock);
        vm
    }

    pub fn interpret(&mut self, code: &str) -> VmRes {
        let mut bytecode_gen = ByteCodeGen::new(code, self.flags.disassemble_compiled);

        let function = match bytecode_gen.compile() {
            Ok(f) => Rc::new(f),
            Err(_) => return Err(VmErr::Compile),
        };

        self.push(Value::Fn(function.clone()));

        self.frames.push(CallFrame {
            closure: Closure::from_fn(&function),
            ip: 0,
            slots: 0,
        });

        self.run()
    }

    fn run(&mut self) -> VmRes {
        loop {
            if self.flags.disassemble_instructions {
                if self.flags.print_stack {
                    print!("          ");
                    self.stack.iter().for_each(|v| print!("[ {} ] ", v));
                    println!();
                }

                let frame = self.frame();
                disassemble_instruction(&frame.closure.function.chunk, frame.ip);
            }

            let op = self.frame().closure.function.chunk.code[self.frame().ip];
            self.frame_mut().ip += 1;

            match op {
                Op::Return => {
                    let res = self.pop();
                    let old = self.frames.pop().unwrap();

                    // Close all upvalues that must live longer than the function
                    self.close_upvalue(old.slots);

                    if self.frames.is_empty() {
                        // Fictive 'main' function
                        self.pop();
                        return Ok(())
                    }
                    
                    // Goes back to before fn call + args
                    self.stack.truncate(old.slots);
                    self.push(res);
                },
                Op::Constant(idx) => {
                    let val = self.frame().closure.function.chunk.constants[idx as usize].clone();
                    self.push(val);
                }
                Op::Negate => {
                    if let Err(e) = self.peek_mut(0).negate() {
                        self.runtime_err(&e.to_string())
                    }
                }
                Op::Add => self.binop(|a, b| a.add(b)),
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
                Op::Print => println!("{}", self.pop()),
                Op::Pop => {
                    self.pop();
                }
                Op::DefineGlobal(idx) => match &self.frame().closure.function.chunk.constants[idx as usize]
                {
                    Value::Str(s) => {
                        let name = *s.clone();
                        let value = self.pop();
                        self.globals.insert(name, value);
                    }
                    _ => panic!("Internal error, using non-string operand to OP_DEFINE_GLOBAL"),
                },
                Op::GetGlobal(idx) => match &self.frame().closure.function.chunk.constants[idx as usize] {
                    Value::Str(s) => match self.globals.get(s.as_ref()) {
                        Some(glob) => self.push(glob.clone()),
                        None => {
                            self.runtime_err(&format!("Undefined variable '{}'", s));
                            return Err(VmErr::Runtime);
                        }
                    },
                    _ => panic!("Internal error, using non-string operand to OP_DEFINE_GLOBAL"),
                },
                Op::SetGlobal(idx) => {
                    let name = {
                        match &self.frame().closure.function.chunk.constants[idx as usize] {
                            Value::Str(s) => *s.clone(),
                            _ => panic!(
                                "Internal error, using non-string operand to OP_DEFINE_GLOBAL"
                            ),
                        }
                    };

                    // FIXME: Check before inserting
                    if self
                        .globals
                        .insert(name.clone(), self.peek(0).clone())
                        .is_none()
                    {
                        self.runtime_err(&format!("Undefined variable '{}'", name));
                        return Err(VmErr::Runtime);
                    }
                }
                Op::GetLocal(idx) => {
                    let offset = self.frame().slots;
                    self.push(self.stack[idx as usize + offset].clone());
                }
                Op::SetLocal(idx) => {
                    let offset = self.frame().slots;
                    self.stack[idx as usize + offset] = self.peek(0).clone();
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
                        self.push(Value::Iter(0..i));
                    } else {
                        self.runtime_err("Range must be an integer");
                        return Err(VmErr::Runtime);
                    }
                }
                Op::ForIter(iter, idx) => {
                    let iter_idx = self.frame().slots + iter as usize;
                    
                    if let Value::Iter(iter) = &mut self.stack[iter_idx] {
                        match iter.next() {
                            Some(v) => {
                                if let Value::Int(i) = &mut self.stack[iter_idx - 1] {
                                    *i = v;
                                } else {
                                    self.runtime_err("failed to get next integer iterator value");
                                    return Err(VmErr::Runtime);
                                }
                            }
                            None => self.frame_mut().ip += idx as usize
                        }
                    } else {
                        panic!("failed to find iterator: {}", &self.stack[iter_idx])
                    }
                }
                Op::Call(args_count) => {
                    let callee = self.peek(args_count as usize).clone();

                    if let Err(e) = self.call_value(callee, args_count) {
                        eprintln!("{}", e.to_string());
                        return Err(VmErr::Runtime);
                    }
                }
                Op::Closure(idx) => {
                    let function = if let Value::Fn(f) = &self.frame().closure.function.chunk.constants[idx as usize] {
                        Rc::clone(f)
                    } else {
                        panic!("closure instruction without function value")
                    };

                    let mut closure = Closure::from_fn(&function);

                    for FnUpValue { index, is_local } in &function.upvalues {
                        if *is_local {
                            closure.upvalues.push(self.capture_value(self.frame().slots + *index as usize));
                        } else {
                            // OP_CLOSURE emitted at the end of fn declaration so we are in the surrounding one
                            closure.upvalues.push(Rc::clone(&self.frame().closure.upvalues[*index as usize]));
                        }
                    }

                    self.push(Value::ClosureFn(closure));
                },
                Op::GetUpValue(idx) => {
                    let val = {
                        let upval = self.frame().closure.upvalues[idx as usize].borrow();

                        match &upval.closed {
                            Some(v) => v.clone(),
                            None => self.stack[upval.location].clone(),
                        }
                    };

                    self.push(val);
                }
                Op::SetUpValue(idx) => {
                    let upval = Rc::clone(&self.frame().closure.upvalues[idx as usize]);
                    let mut upval = upval.borrow_mut();

                    let val_assign = self.peek(0).clone();

                    if upval.closed.is_none() {
                        self.stack[upval.location] = val_assign;
                    } else {
                        upval.closed = Some(val_assign);
                    }
                },
                Op::CloseUpValue => {
                    self.close_upvalue(self.stack.len() - 1);
                    self.pop();
                },
                Op::Struct(idx) => {
                    let name = if let Value::Str(s) = &self.frame().closure.function.chunk.constants[idx as usize] {
                        s
                    } else {
                        self.runtime_err("Unable to find structure name");
                        return Err(VmErr::Runtime);
                    };

                    let structure = Struct::new(name);
                    let index = self.frame().closure.structures.len();
                    self.frame_mut().closure.structures.push(structure);
                    self.push(Value::new_struct_id(index));
                },
                Op::GetProperty(idx) => {
                    let inst = if let Value::Instance(inst_id) = self.pop() {
                        &self.frame().closure.instances[inst_id.index]
                    } else {
                        self.runtime_err("only instances have field");
                        return Err(VmErr::Runtime);
                    };

                    let name = if let Value::Str(s) = &self.frame().closure.function.chunk.constants[idx as usize] {
                        s.clone()
                    } else {
                        self.runtime_err("property access must be with identifer");
                        return Err(VmErr::Runtime);
                    };

                    match inst.fields.get(&**name) {
                        Some(value) => self.push(value.clone()),
                        None => self.bind_method(inst.struct_id, &*name)?,
                    }
                },
                Op::SetProperty(idx) => {
                    let value = self.pop();
                    let field_name = if let Value::Str(s) = &self.frame().closure.function.chunk.constants[idx as usize] {
                        s.clone()
                    } else {
                        self.runtime_err("property access must be with identifer");
                        return Err(VmErr::Runtime);
                    };

                    if let Value::Instance(inst_id) =  self.pop() {
                        let inst = &mut self.frame_mut().closure.instances[inst_id.index];

                        match inst.fields.insert(*field_name, value.clone()) {
                            Some(_) => {},
                            None => eprintln!("Instance dosen't have field"),
                        }
                    }

                    // We put the assignement value back on top to return it as:
                    //  print toast.jam = "grape" // Prints "grape".
                    self.push(value);
                },
                Op::Method(idx) => {
                    let name = if let Value::Str(s) = &self.frame().closure.function.chunk.constants[idx as usize] {
                        s.clone()
                    } else {
                        self.runtime_err("method name must be an identifier");
                        return Err(VmErr::Runtime);
                    };

                    if let Err(e) = self.define_method(&name) {
                        eprintln!("{}", e.to_string());
                        return Err(VmErr::Runtime);
                    }
                },
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
            Value::ClosureFn(f) => return self.call(f, args_count),
            Value::NativeFn(f) => {
                let res = f(args_count as usize, (self.stack.len() - 1) - args_count as usize);
                self.stack.truncate(self.stack.len() - args_count as usize + 1);
                self.push(res);
            }
            Value::Struct(struct_id) => {
                let idx = self.frame().closure.instances.len();
                let stack_len = self.stack.len();
                let instance = Instance::new(struct_id.index);

                self.frame_mut().closure.instances.push(instance);
                self.stack[(stack_len - 1) - args_count as usize] = Value::new_instance_id(idx);
            }
            Value::BoundMethod(method) => {
                return self.call(method.method, args_count)
            }
            _ => {
                self.runtime_err("can only call functions and structures");
                return Err(VmErr::Runtime)
            }
        }

        Ok(())
    }

    fn call(&mut self, closure: Closure, args_count: u8) -> VmRes {
        if self.frames.len() == Self::FRAMES_MAX {
            self.runtime_err("stack overflow");
            return Err(VmErr::Runtime)
        }

        if closure.function.arity != args_count as usize {
            self.runtime_err(&format!(
                "expected {} arguments but got {}",
                closure.function.arity, args_count
            ));

            return Err(VmErr::Runtime)
        }

        self.frames.push(CallFrame {
            closure,
            ip: 0,
            slots: self.stack.len() - args_count as usize - 1, // -1 to get the fn call
        });

        Ok(())
    }

    fn define_method(&mut self, name: &str) -> VmRes {
        let method = self.pop();

        let idx = if let Value::Struct(struct_id) = self.peek(0) {
            struct_id.index
        } else {
            self.runtime_err("can only call functions and structures");
            return Err(VmErr::Runtime)
        };

        let structure = &mut self.frame_mut().closure.structures[idx];
        structure.methods.insert(name.into(), method);
        Ok(())
    }

    fn bind_method(&mut self, struct_id: usize, name: &String) -> Result<(), VmErr> {
        let receiver = self.pop();
        let structure = &self.frame().closure.structures[struct_id];

        match structure.methods.get(name) {
            Some(method) => {
                if let Value::ClosureFn(closure) = method {
                    let bound_method = Value::new_bound_method(receiver, closure);
                    self.push(bound_method);

                    return Ok(())
                }
            },
            None => {}
        }

        self.runtime_err(&format!("instance dosen't have field {}", name));
        Err(VmErr::Runtime)
    }

    fn capture_value(&mut self, index: usize) -> Rc<RefCell<UpValue>> {
        // Ensure only one UpValue pointing to a slot
        for upval in self.open_upvalues.iter().rev() {
            if upval.borrow().location < index {
                break
            }
            
            if upval.borrow().location == index {
                return Rc::clone(upval)
            }
        }

        let new_upval = Rc::new(RefCell::new(UpValue::new(index)));
        self.open_upvalues.push(Rc::clone(&new_upval));
        new_upval
    }

    // Close all upvalues that point to slot above it in the stack
    fn close_upvalue(&mut self, last: usize) {
        let mut i = self.open_upvalues.len();

        loop {
            if i < 1 {
                break
            }

            let mut upval = self.open_upvalues[i - 1].borrow_mut();

            if upval.location < last {
                break
            }

            upval.closed = Some(self.stack[upval.location].clone());

            i -= 1;
        }

        self.open_upvalues.truncate(i);
    }

    fn define_native(&mut self, name: &str, function: NativeFunction) {
        self.globals.insert(name.into(), Value::NativeFn(function));
    }

    fn pop(&mut self) -> Value {
        self.stack.pop().unwrap()
    }

    fn push(&mut self, value: Value) {
        self.stack.push(value);
    }

    // PERF: Use [len() -1] to avoid unwraping for performance
    fn frame(&self) -> &CallFrame {
        self.frames.last().unwrap()
    }

    fn frame_mut(&mut self) -> &mut CallFrame {
        self.frames.last_mut().unwrap()
    }

    // PERF: Cache stack.len()?
    fn peek(&self, distance: usize) -> &Value {
        &self.stack[self.stack.len() - distance - 1]
    }

    fn peek_mut(&mut self, distance: usize) -> &mut Value {
        let idx = self.stack.len() - distance - 1;
        &mut self.stack[idx]
    }

    fn runtime_err(&self, msg: &str) {
        eprintln!(
            "[line {}] Error: {}",
            self.frame().closure.function.chunk.lines[self.frame().closure.function.chunk.code.len() - 1] + 1,
            msg
        );

        for frame in self.frames.iter().rev() {
            eprint!(
                "[line {}] in ",
                frame.closure.function.chunk.lines[frame.ip] + 1,
            );

            if frame.closure.function.name.is_empty() {
                eprintln!("script");
            } else {
                eprintln!("{}()", frame.closure.function.name);
            }
        }
    }
}
