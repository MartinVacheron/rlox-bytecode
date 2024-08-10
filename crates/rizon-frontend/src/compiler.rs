use crate::{
    chunk::{Chunk, Op},
    debug::disassemble,
    lexer::{Lexer, Token, TokenKind},
    value::Value,
};

use Precedence as P;

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

struct Parser<'src> {
    lexer: Lexer<'src>,
    current: Token<'src>,
    previous: Token<'src>,
    had_error: bool,
    panic_mode: bool,
}

impl<'src> Parser<'src> {
    pub fn new(code: &'src str) -> Self {
        Self {
            lexer: Lexer::new(code),
            current: Token::default(),
            previous: Token::default(),
            had_error: false,
            panic_mode: false,
        }
    }

    fn advance(&mut self) {
        std::mem::swap(&mut self.previous, &mut self.current);

        loop {
            self.current = self.lexer.lex();

            if self.current.kind != TokenKind::Error {
                break;
            }

            self.had_error = true;
            self.panic_mode = true;
            self.error_at_current(&self.current.lexeme)
        }
    }

    fn expect(&mut self, kind: TokenKind, msg: &str) {
        if self.current.kind == kind {
            self.advance();
            return;
        }

        self.error_at_current(msg);
    }

    fn error_at_current(&self, msg: &str) {
        self.error_at(&self.current, msg);
    }

    fn error(&self, msg: &str) {
        self.error_at(&self.previous, msg);
    }

    fn error_at(&self, token: &Token, msg: &str) {
        if self.panic_mode {
            return;
        }

        eprint!("[line {}] Error", token.line);

        match token.kind {
            TokenKind::Eof => eprint!(" at end"),
            TokenKind::Error => {}
            _ => eprint!(" at '{}'", token.lexeme),
        }

        eprintln!(": {}", msg);
    }
}

type CompilerFn<'src> = fn(&mut Compiler<'src>);

struct Rule<'src> {
    prefix: Option<CompilerFn<'src>>,
    infix: Option<CompilerFn<'src>>,
    precedence: Precedence,
}

impl<'src> Rule<'src> {
    pub fn new(
        prefix: Option<CompilerFn<'src>>,
        infix: Option<CompilerFn<'src>>,
        precedence: Precedence,
    ) -> Self {
        Self {
            prefix,
            infix,
            precedence,
        }
    }
}

type Rules<'src> = [Rule<'src>; 42];

fn make_rules<'src>() -> Rules<'src> {
    [
        Rule::new(Some(Compiler::grouping), None, P::None),                // LeftParen
        Rule::new(None, None, P::None),                                    // RightParen
        Rule::new(None, None, P::None),                                    // LeftBrace
        Rule::new(None, None, P::None),                                    // RightBrace
        Rule::new(None, None, P::None),                                    // Comma
        Rule::new(None, None, P::None),                                    // Dot
        Rule::new(None, None, P::None),                                    // Colon
        Rule::new(Some(Compiler::unary), Some(Compiler::binary), P::Term), // Minus
        Rule::new(None, Some(Compiler::binary), P::Term),                  // Plus
        Rule::new(None, Some(Compiler::binary), P::Factor),                // Slash
        Rule::new(None, Some(Compiler::binary), P::Factor),                // Star
        Rule::new(Some(Compiler::unary), None, P::None),                   // Bang
        Rule::new(None, Some(Compiler::binary), P::Equality),              // BangEqual
        Rule::new(None, None, P::None),                                    // Equal
        Rule::new(None, Some(Compiler::binary), P::Equality),              // EqualEqual
        Rule::new(None, Some(Compiler::binary), P::Comparison),            // Greater
        Rule::new(None, Some(Compiler::binary), P::Comparison),            // GreaterEqual
        Rule::new(None, Some(Compiler::binary), P::Comparison),            // Less
        Rule::new(None, Some(Compiler::binary), P::Comparison),            // LessEqual
        Rule::new(None, None, P::None),                                    // Identifier
        Rule::new(Some(Compiler::string), None, P::None),                  // String
        Rule::new(Some(Compiler::number), None, P::None),                  // Int
        Rule::new(Some(Compiler::number), None, P::None),                  // Float
        Rule::new(None, None, P::None),                                    // Struct
        Rule::new(None, None, P::None),                                    // Fn
        Rule::new(None, None, P::None),                                    // SelfKw
        Rule::new(None, None, P::None),                                    // Var
        Rule::new(None, None, P::None),                                    // Return
        Rule::new(None, None, P::None),                                    // If
        Rule::new(None, None, P::None),                                    // Else
        Rule::new(None, None, P::None),                                    // And
        Rule::new(None, None, P::None),                                    // Or
        Rule::new(Some(Compiler::literal), None, P::None),                 // Null
        Rule::new(None, None, P::None),                                    // Print
        Rule::new(None, None, P::None),                                    // For
        Rule::new(None, None, P::None),                                    // In
        Rule::new(None, None, P::None),                                    // While
        Rule::new(Some(Compiler::literal), None, P::None),                 // True
        Rule::new(Some(Compiler::literal), None, P::None),                 // False
        Rule::new(None, None, P::None),                                    // NewLine
        Rule::new(None, None, P::None),                                    // Error
        Rule::new(None, None, P::None),                                    // Eof
    ]
}

pub struct Compiler<'src> {
    parser: Parser<'src>,
    chunk: &'src mut Chunk,
    rules: Rules<'src>,
}

impl<'src> Compiler<'src> {
    pub fn new(code: &'src str, chunk: &'src mut Chunk) -> Self {
        Self {
            parser: Parser::new(code),
            chunk,
            rules: make_rules(),
        }
    }

    pub fn compile(&mut self, debug: bool) -> bool {
        self.parser.advance();

        self.expression();

        while self.parser.current.kind == TokenKind::NewLine {
            self.parser.advance();
        }

        self.parser.expect(TokenKind::Eof, "");

        // Tmp
        self.emit_byte(Op::Return);

        if debug {
            disassemble(&self.chunk, "code");
        }

        return self.parser.had_error;
    }

    fn expression(&mut self) {
        self.parse_precedence(P::Assignment);
    }

    fn parse_precedence(&mut self, precedence: Precedence) {
        self.parser.advance();

        let prefix_rule = self.get_rule(self.parser.previous.kind).prefix;

        match prefix_rule {
            Some(f) => f(self),
            None => {
                self.parser.error("Expect expression");
                return;
            }
        }

        while precedence <= self.get_rule(self.parser.current.kind).precedence {
            self.parser.advance();

            match self.get_rule(self.parser.previous.kind).infix {
                Some(f) => f(self),
                None => unreachable!(),
            }
        }
    }

    fn binary(&mut self) {
        let operator = self.parser.previous.kind;
        let rule = self.get_rule(operator);
        self.parse_precedence(rule.precedence.next());

        match operator {
            TokenKind::Plus => self.emit_byte(Op::Add),
            TokenKind::Minus => self.emit_byte(Op::Subtract),
            TokenKind::Star => self.emit_byte(Op::Multiply),
            TokenKind::Slash => self.emit_byte(Op::Divide),
            TokenKind::BangEqual => self.emit_bytes(Op::Equal, Op::Not),
            TokenKind::EqualEqual => self.emit_byte(Op::Equal),
            TokenKind::Greater => self.emit_byte(Op::Greater),
            TokenKind::GreaterEqual => self.emit_bytes(Op::Less, Op::Not),
            TokenKind::Less => self.emit_byte(Op::Less),
            TokenKind::LessEqual => self.emit_bytes(Op::Greater, Op::Not),
            _ => unreachable!(),
        }
    }

    fn grouping(&mut self) {
        self.expression();

        self.parser
            .expect(TokenKind::RightParen, "Expected ')' after expression");
    }

    fn unary(&mut self) {
        let operator = self.parser.previous.kind;

        self.parse_precedence(P::Unary);

        // Emit negate after operand so that we pop the right value from stack
        match operator {
            TokenKind::Minus => self.emit_byte(Op::Negate),
            TokenKind::Bang => self.emit_byte(Op::Not),
            _ => unreachable!(),
        }
    }

    fn number(&mut self) {
        let val = self
            .parser
            .previous
            .lexeme
            .parse::<f64>()
            .expect("Error parsing float");

        self.emit_constant(Value::Float(val));
    }

    fn string(&mut self) {
        let str_len = self.parser.previous.lexeme.len();
        let final_str = self.parser.previous.lexeme[1..str_len-1].to_string();
        self.emit_constant(Value::Str(Box::new(final_str)));
    }

    fn literal(&mut self) {
        match self.parser.previous.kind {
            TokenKind::True => self.emit_byte(Op::True),
            TokenKind::False => self.emit_byte(Op::False),
            TokenKind::Null => self.emit_byte(Op::Null),
            _ => unreachable!()
        }
    }

    fn make_constant(&mut self, value: Value) -> usize {
        let id = self.chunk.write_constant(value);

        if id > usize::MAX {
            self.parser.error("Too many constant in one chunk");
            return 0;
        }

        id
    }

    fn get_rule(&self, kind: TokenKind) -> &Rule<'src> {
        &self.rules[kind as usize]
    }

    fn emit_constant(&mut self, value: Value) {
        let id = self.make_constant(value) as u8;
        self.emit_byte(Op::Constant(id));
    }

    fn emit_bytes(&mut self, byte1: Op, byte2: Op) {
        self.emit_byte(byte1);
        self.emit_byte(byte2);
    }

    fn emit_byte(&mut self, byte: Op) {
        self.chunk.write(byte, self.parser.previous.line);
    }
}
