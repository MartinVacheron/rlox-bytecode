use anyhow::{bail, Result};

use crate::{
    chunk::{Chunk, Op},
    debug::disassemble,
    lexer::{Lexer, Token, TokenKind},
    value::Function,
};

mod backend;
mod frontend;
mod rules;

use rules::{make_rules, Rules};

#[derive(PartialEq, PartialOrd)]
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
}

const MAX_LOCALS: usize = std::u8::MAX as usize + 1;

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
}

struct Compiler<'src> {
    function: Function,
    fn_type: FnType,
    scope: Scope<'src>,
    enclosing: Option<Box<Compiler<'src>>>,
}

impl<'src> Compiler<'src> {
    pub fn new(fn_type: FnType) -> Box<Self> {
        let mut scope = Scope::default();
        scope.locals.push(Local { name: "", depth: 0 });

        Box::new(Self {
            fn_type,
            scope,
            enclosing: None,
            function: Function::default(),
        })
    }
}


pub struct ByteCodeGen<'src> {
    debug: bool,
    // Frontend
    lexer: Lexer<'src>,
    current: Token<'src>,
    previous: Token<'src>,
    had_error: bool,
    panic_mode: bool,
    // Backend
    rules: Rules<'src>,
    // TODO: Box needed?
    compiler: Box<Compiler<'src>>,
}

impl<'src> ByteCodeGen<'src> {
    pub fn new(code: &'src str, debug: bool) -> Self {
        Self {
            debug,
            lexer: Lexer::new(code),
            current: Token::default(),
            previous: Token::default(),
            had_error: false,
            panic_mode: false,
            rules: make_rules(),
            compiler: Compiler::new(FnType::Script),
        }
    }

    pub fn compile(&mut self) -> Result<Function> {
        self.advance();

        while !self.is_at(TokenKind::Eof) {
            self.declaration();

            self.skip_new_lines();
        }

        self.expect(TokenKind::Eof, "");

        self.emit_return();

        if self.had_error {
            bail!("Compile error")
        } else {
            if self.debug {
                self.dis_compiler();
            }

            Ok(self.end_compiler())
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
                self.compiler.scope.locals.pop();

                self.emit_byte(Op::Pop);
            } else {
                break;
            }
        }
    }

    fn add_local(&mut self, name: &'src str) {
        let local = Local { name, depth: -1 };

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
        let old = std::mem::replace(&mut self.compiler, Compiler::new(fn_type));
        self.compiler.enclosing = Some(old);
        self.compiler.function.name = self.previous.lexeme.to_string();
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

    fn end_compiler(&mut self) -> Function {
        std::mem::take(&mut self.compiler.function)
    }

    fn dis_compiler(&self) {
        let name = if self.compiler.function.name.is_empty() {
            "<script>"
        } else {
            &self.compiler.function.name
        };

        disassemble(self.chunk(), name);
    }
}
