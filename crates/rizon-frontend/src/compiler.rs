use anyhow::{bail, Result};

use crate::{
    chunk::{Chunk, Op},
    debug::disassemble,
    lexer::{Lexer, Token, TokenKind}, value::{Function, Value},
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

enum FnType {
    Script,
    Function,
}

pub struct Compiler<'src> {
    // Frontend
    lexer: Lexer<'src>,
    current: Token<'src>,
    previous: Token<'src>,
    had_error: bool,
    panic_mode: bool,
    // Backend
    rules: Rules<'src>,
    function: Function,
    fn_type: FnType,
    scope: Scope<'src>,
}

impl<'src> Compiler<'src> {
    pub fn new(code: &'src str) -> Self {
        let mut scope = Scope::default();
        scope.locals.push(Local { name: "", depth: 0 });

        Self {
            lexer: Lexer::new(code),
            current: Token::default(),
            previous: Token::default(),
            had_error: false,
            panic_mode: false,
            rules: make_rules(),
            function: Function::default(),
            fn_type: FnType::Script,
            scope,
        }
    }

    pub fn compile(&mut self, debug: bool) -> Result<&Function> {
        self.advance();

        while !self.is_at(TokenKind::Eof) {
            self.declaration();

            self.skip_new_lines();
        }

        self.expect(TokenKind::Eof, "");

        // Tmp
        let previous_line = self.previous.line;
        self.chunk_mut().write(Op::Return, previous_line);

        if debug {
            let name = if self.function.name.is_empty() { "<script>" } else { &self.function.name };
            
            disassemble(self.chunk(), name);
        }
        
        if self.had_error {
            bail!("Compile error")
        } else {
            Ok(&self.function)
        }
    }

    fn chunk(&self) -> &Chunk {
        &self.function.chunk
    }

    fn chunk_mut(&mut self) -> &mut Chunk {
        &mut self.function.chunk
    }

    fn chunk_last_idx(&mut self) -> usize {
        self.chunk().code.len() - 1
    }

    fn begin_scope(&mut self) {
        self.scope.depth += 1;
    }

    fn end_scope(&mut self) {
        self.scope.depth -= 1;

        for idx in (0..self.scope.locals.len()).rev() {
            if self.scope.locals[idx].depth > self.scope.depth {
                self.scope.locals.pop();

                self.emit_byte(Op::Pop);
            } else {
                break
            }
        }
    }

    fn add_local(&mut self, name: &'src str) {
        let local = Local {
            name,
            depth: -1,
        };

        self.scope.locals.push(local);
    }

    fn mark_initialized(&mut self) {
        let last_id = self.scope.locals.len() - 1;
        self.scope.locals[last_id].depth = self.scope.depth;
    }
}
