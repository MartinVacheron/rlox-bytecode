use std::ops::Deref;

use anyhow::{bail, Result};

use crate::{
    chunk::{Chunk, Op}, debug::Disassembler, gc::{Gc, GcRef}, lexer::{Lexer, Token, TokenKind}, object::Function
};

mod backend;
mod frontend;
mod rules;

use rules::{make_rules, Rules};

#[derive(PartialEq, PartialOrd, Debug)]
enum Precedence {
    None,
    Assignment, // =
    Or,         // or
    And,        // and
    Equality,   // == !=
    Comparison, // < > <= >=
    Term,       // + -
    Factor,     // * /
    Unary,      // ! -
    Call,       // . ()
    Primary,
}

impl Precedence {
    pub fn next(&self) -> Self {
        match self {
            Precedence::None => Precedence::Assignment,
            Precedence::Assignment => Precedence::Or,
            Precedence::Or => Precedence::And,
            Precedence::And => Precedence::Equality,
            Precedence::Equality => Precedence::Comparison,
            Precedence::Comparison => Precedence::Term,
            Precedence::Term => Precedence::Factor,
            Precedence::Factor => Precedence::Unary,
            Precedence::Unary => Precedence::Call,
            Precedence::Call => Precedence::Primary,
            Precedence::Primary => Precedence::None,
        }
    }
}

#[derive(Default, Debug, Copy, Clone)]
struct Local<'src> {
    name: &'src str,
    depth: i16,
    is_captured: bool,
}

const MAX_LOCALS: usize = std::u8::MAX as usize + 1;

#[derive(Debug)]
struct Scope<'src> {
    locals: Vec<Local<'src>>,
    depth: i16,
}

impl<'src> Default for Scope<'src> {
    fn default() -> Self {
        Self {
            locals: Vec::with_capacity(MAX_LOCALS),
            depth: 0,
        }
    }
}

#[derive(Default, PartialEq)]
enum FnType {
    #[default]
    Script,
    Function,
    Method,
    Initializer,
}


struct Compiler<'src> {
    function: Function,
    fn_type: FnType,
    scope: Scope<'src>,
    enclosing: Option<Box<Compiler<'src>>>,
}

impl<'src> Compiler<'src> {
    const LOCAL_COUNT: usize = u8::MAX as usize + 1;

    pub fn new(function_name: GcRef<String>, fn_type: FnType) -> Box<Self> {
        let mut scope = Scope::default();

        let first_slot = if fn_type == FnType::Function { "" } else { "self" };
        // For "self"
        scope.locals.push(Local { name: first_slot, depth: 0, is_captured: false });

        Box::new(Self {
            fn_type,
            scope,
            enclosing: None,
            function: Function::new(function_name),
        })
    }

    fn resolve_local(&self, name: &str) -> Option<u8> {
        for (idx, local) in self.scope.locals.iter().enumerate().rev() {
            if local.name == name {
                if local.depth == -1 {
                    println!("error: can't use local variable in its own initializer");
                }

                return Some(idx as u8)
            }
        }

        None
    }

    pub fn resolve_upvalue(&mut self, name: &str) -> Option<u8> {
        if let Some(enclosing) = &mut self.enclosing {
            if let Some(local_id) = enclosing.resolve_local(name) {
                // We put the information in enclosing compiler
                enclosing.scope.locals[local_id as usize].is_captured = true;
                return Some(self.add_upvalue(local_id, true))
            }

            if let Some(upval_id) = enclosing.resolve_upvalue(name) {
                return Some(self.add_upvalue(upval_id, false))
            }
        }

        None
    }

    fn add_upvalue(&mut self, idx: u8, is_local: bool) -> u8 {
        let upvalue_count = self.function.upvalues.len();

        for (i, upvalue) in self.function.upvalues.iter().enumerate() {
            if upvalue.index == idx && upvalue.is_local == is_local {
                return i as u8
            }
        }

        if upvalue_count == Self::LOCAL_COUNT {
            println!("too many closure variables in function")
        }

        self.function.add_upvalue(idx, is_local);

        upvalue_count as u8
    }
}


#[derive(Debug)]
struct StructCompiler {
    enclosing: Option<Box<StructCompiler>>,
}

impl StructCompiler {
    pub fn new(enclosing: Option<Box<StructCompiler>>) -> Box<Self> {
        Box::new(Self { enclosing })
    }
}

pub struct ByteCodeGen<'src> {
    // Frontend
    lexer: Lexer<'src>,
    current: Token<'src>,
    previous: Token<'src>,
    had_error: bool,
    panic_mode: bool,
    debug: bool,
    // Backend
    rules: Rules<'src>,
    compiler: Box<Compiler<'src>>,
    gc: &'src mut Gc,
    current_struct: Option<Box<StructCompiler>>,
}

impl<'src> ByteCodeGen<'src> {
    pub fn new(code: &'src str, gc: &'src mut Gc, debug: bool) -> Self {
        let name = gc.intern("script".into());

        Self {
            lexer: Lexer::new(code),
            current: Token::default(),
            previous: Token::default(),
            had_error: false,
            panic_mode: false,
            debug,
            rules: make_rules(),
            compiler: Compiler::new(name, FnType::Script),
            gc,
            current_struct: None,
        }
    }

    pub fn compile(mut self) -> Result<GcRef<Function>> {
        self.advance();

        while !self.is_at(TokenKind::Eof) {
            self.declaration();

            self.skip_new_lines();
        }

        self.expect(TokenKind::Eof, "");
        self.emit_return();

        if self.debug {
            self.dis_compiler();
        }

        if self.had_error {
            bail!("Compile error")
        } else {
            Ok(self.gc.alloc(self.compiler.function))
        }
    }

    fn chunk(&self) -> &Chunk {
        &self.compiler.function.chunk
    }

    fn chunk_mut(&mut self) -> &mut Chunk {
        &mut self.compiler.function.chunk
    }

    fn chunk_last_idx(&mut self) -> usize {
        self.chunk().code.len() - 1
    }

    fn begin_scope(&mut self) {
        self.compiler.scope.depth += 1;
    }

    fn end_scope(&mut self) {
        self.compiler.scope.depth -= 1;

        for idx in (0..self.compiler.scope.locals.len()).rev() {
            if self.compiler.scope.locals[idx].depth > self.compiler.scope.depth {
                if self.compiler.scope.locals[idx].is_captured {
                    self.emit_byte(Op::CloseUpValue);
                } else {
                    self.emit_byte(Op::Pop);
                }

                self.compiler.scope.locals.pop();
            } else {
                break;
            }
        }
    }

    fn add_local(&mut self, name: &'src str) {
        let local = Local { name, depth: -1, is_captured: false };

        self.compiler.scope.locals.push(local);
    }

    fn mark_initialized(&mut self) {
        if self.compiler.scope.depth == 0 {
            return;
        }

        let last_id = self.compiler.scope.locals.len() - 1;
        self.compiler.scope.locals[last_id].depth = self.compiler.scope.depth;
    }

    fn push_compiler(&mut self, fn_type: FnType) {
        let name = self.gc.intern(self.previous.lexeme.to_string());
        let old = std::mem::replace(&mut self.compiler, Compiler::new(name, fn_type));
        self.compiler.enclosing = Some(old);
    }

    fn pop_compiler(&mut self) -> Function {
        // We add return statement
        self.emit_return();

        if self.debug {
            self.dis_compiler();
        }
        
        match self.compiler.enclosing.take() {
            Some(enclosing) => {
                let old = std::mem::replace(&mut self.compiler, enclosing);
                old.function
            }
            None => panic!("can't find enclosing compiler")
        }
    }

    fn dis_compiler(&self) {
        // let name = self.gc.deref(&self.compiler.function.name);
        let name = self.compiler.function.name.deref();
        let name = if name.is_empty() {
            "<script>"
        } else {
            name
        };

        let disassembler = Disassembler::new(&self.compiler.function.chunk, None);

        disassembler.disassemble(name);
        println!();
    }
}
