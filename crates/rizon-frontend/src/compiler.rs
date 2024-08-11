use crate::{
    chunk::{Chunk, Op},
    debug::disassemble,
    lexer::{Lexer, Token, TokenKind},
};

mod rules;
mod frontend;
mod backend;

use rules::{Rules, make_rules};


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

#[derive(Default, Copy, Clone)]
struct Local<'src> {
    name: &'src str,
    depth: usize,
}

struct Scope<'src> {
    locals: [Local<'src>; 256],
    local_count: usize,
    depth: usize,
}

impl<'src> Default for Scope<'src> {
    fn default() -> Self {
        Self {
            locals: [Local::default(); 256],
            local_count: 0,
            depth: 0,
        }
    }
}

pub struct Compiler<'src> {
    // Frontend
    lexer: Lexer<'src>,
    current: Token<'src>,
    previous: Token<'src>,
    had_error: bool,
    panic_mode: bool,
    // Backend
    chunk: &'src mut Chunk,
    rules: Rules<'src>,
    scope: Scope<'src>,
}

impl<'src> Compiler<'src> {
    pub fn new(code: &'src str, chunk: &'src mut Chunk) -> Self {
        Self {
            lexer: Lexer::new(code),
            current: Token::default(),
            previous: Token::default(),
            had_error: false,
            panic_mode: false,
            chunk,
            rules: make_rules(),
            scope: Scope::default(),
        }
    }

    pub fn compile(&mut self, debug: bool) -> bool {
        self.advance();

        while !self.is_at(TokenKind::Eof) {
            self.declaration();
    
            self.skip_new_lines();
        }

        self.expect(TokenKind::Eof, "");

        // Tmp
        self.chunk.write(Op::Return, self.previous.line);

        if debug {
            disassemble(&self.chunk, "code");
        }

        return self.had_error;
    }

    fn begin_scope(&mut self) {
        self.scope.depth += 1;
    }

    fn end_scope(&mut self) {
        self.scope.depth -= 1;
    }

    fn add_local(&mut self, name: &'src str) {
        let local = Local { name, depth: self.scope.depth };
        self.scope.local_count += 1;

        self.scope.locals[self.scope.local_count] = local;
    }
}
