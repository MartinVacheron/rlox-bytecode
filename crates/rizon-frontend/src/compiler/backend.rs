use crate::{chunk::Op, lexer::TokenKind, value::Value};

use super::{rules::Rule, Compiler, Precedence};

use Precedence as P;


impl<'src> Compiler<'src> {
    pub(super) fn declaration(&mut self) {
        if self.is_at(TokenKind::Var) {
            self.var_declaration();
        } else {
            self.statement();
        }

        if self.panic_mode {
            self.synchronize();
        }
    }

    fn var_declaration(&mut self) {
        let id = self.parse_variable("expect variable name");

        if self.is_at(TokenKind::Equal) {
            self.expression();
        } else {
            self.emit_byte(Op::Null);
        }

        self.expect_or_eof(TokenKind::NewLine, "expected end of line after var declaration");
        self.define_variable(id);
    }

    fn parse_variable(&mut self, msg: &str) -> usize {
        self.expect(TokenKind::Identifier, msg);

        self.declare_variable();

        if self.scope.depth > 0 {
            return 0
        }

        self.identifier_constant()
    }

    fn declare_variable(&mut self) {
        if self.scope.depth == 0 {
            return
        }

        self.add_local(&self.previous.lexeme);
    }

    fn identifier_constant(&mut self) -> usize {
        let name = Value::Str(Box::new(self.previous.lexeme.to_string()));
        self.make_constant(name)
    }

    fn define_variable(&mut self, id: usize) {
        if self.scope.depth > 0 {
            return
        }

        self.emit_byte_prev_line(Op::DefineGlobal(id as u8))
    }

    fn statement(&mut self) {
        if self.is_at(TokenKind::Print) {
            self.print_statement();
        } else if self.is_at(TokenKind::LeftBrace) {
            self.begin_scope();
            self.block();
            self.end_scope();
        } else {
            self.expression_statement();
        }
    }

    fn print_statement(&mut self) {
        self.expression();
        self.emit_byte(Op::Print);
    }

    fn block(&mut self) {
        while !self.check(TokenKind::RightBrace) && !self.eof() {
            self.declaration();
        }

        self.expect(TokenKind::RightBrace, "expect '}' after block");
    }

    fn expression_statement(&mut self) {
        self.expression();
        self.emit_byte(Op::Pop);
    }

    fn expression(&mut self) {
        self.parse_precedence(P::Assignment);
    }

    fn parse_precedence(&mut self, precedence: Precedence) {
        self.advance();
        
        let can_assign = precedence <= P::Assignment;
        let prefix_rule = self.get_rule(self.previous.kind).prefix;

        match prefix_rule {
            Some(f) => f(self, can_assign),
            None => {
                self.error("Expect expression");
                return;
            }
        }

        while precedence <= self.get_rule(self.current.kind).precedence {
            self.advance();

            match self.get_rule(self.previous.kind).infix {
                Some(f) => f(self, can_assign),
                None => unreachable!(),
            }
        }

        if can_assign && self.is_at(TokenKind::Equal) {
            self.error("invalid assignment target");
        }
    }

    pub(super) fn binary(&mut self, _: bool) {
        let operator = self.previous.kind;
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

    pub(super) fn grouping(&mut self, _: bool) {
        self.expression();

        self.expect(TokenKind::RightParen, "Expected ')' after expression");
    }

    pub(super) fn unary(&mut self, _: bool) {
        let operator = self.previous.kind;

        self.parse_precedence(P::Unary);

        // Emit negate after operand so that we pop the right value from stack
        match operator {
            TokenKind::Minus => self.emit_byte(Op::Negate),
            TokenKind::Bang => self.emit_byte(Op::Not),
            _ => unreachable!(),
        }
    }

    pub(super) fn number(&mut self, _: bool) {
        let val = self
            .previous
            .lexeme
            .parse::<f64>()
            .expect("Error parsing float");

        self.emit_constant(Value::Float(val));
    }

    pub(super) fn string(&mut self, _: bool) {
        let str_len = self.previous.lexeme.len();
        let final_str = self.previous.lexeme[1..str_len-1].to_string();
        self.emit_constant(Value::Str(Box::new(final_str)));
    }

    pub(super) fn variable(&mut self, can_assign: bool) {
        self.named_variable(can_assign);
    }

    fn named_variable(&mut self, can_assign: bool) {
        let id = self.identifier_constant();

        if self.is_at(TokenKind::Equal) && can_assign {
            self.expression();
            self.emit_byte(Op::SetGlobal(id as u8));
        } else {
            self.emit_byte(Op::GetGlobal(id as u8));
        }
    }

    pub(super) fn literal(&mut self, _: bool) {
        match self.previous.kind {
            TokenKind::True => self.emit_byte(Op::True),
            TokenKind::False => self.emit_byte(Op::False),
            TokenKind::Null => self.emit_byte(Op::Null),
            _ => unreachable!()
        }
    }

    fn make_constant(&mut self, value: Value) -> usize {
        let id = self.chunk.write_constant(value);

        if id > usize::MAX {
            self.error("Too many constant in one chunk");
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
        self.chunk.write(byte, self.previous.line);
    }

    fn emit_byte_prev_line(&mut self, byte: Op) {
        self.chunk.write(byte, self.previous.line - 1);
    }
}
