use crate::{chunk::Op, lexer::TokenKind, value::Value};

use super::{rules::Rule, Compiler, Precedence};

use Precedence as P;


impl<'src> Compiler<'src> {
    pub(super) fn declaration(&mut self) {
        self.skip_new_lines();

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

        if self.scope.locals.len() > 0 {
            for i in (0..=self.scope.locals.len() - 1).rev() {
                if self.scope.locals[i].depth < self.scope.depth {
                    break
                }
    
                if self.scope.locals[i].name == self.previous.lexeme {
                    self.error("already declared variable in this scope");
                }
            }
        }

        self.add_local(&self.previous.lexeme);
    }

    fn identifier_constant(&mut self) -> usize {
        let name = Value::Str(Box::new(self.previous.lexeme.to_string()));
        self.make_constant(name)
    }

    fn define_variable(&mut self, id: usize) {
        if self.scope.depth > 0 {
            self.mark_initialized();
            return
        }

        self.emit_byte_prev_line(Op::DefineGlobal(id as u8))
    }

    fn statement(&mut self) {
        if self.is_at(TokenKind::Print) {
            self.print_statement();
        } else if self.is_at_and_skip(TokenKind::LeftBrace) {
            self.begin_scope();
            self.block();
            self.end_scope();
        } else if self.is_at(TokenKind::If) {
            self.if_statement();
        } else if self.is_at(TokenKind::While) {
            self.while_statement();
        }
        else {
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
            self.skip_new_lines();
        }

        self.expect(TokenKind::RightBrace, "expect '}' after block");
    }

    fn if_statement(&mut self) {
        self.expression();
        self.expect_and_skip(TokenKind::LeftBrace, "expect '{' before 'if' body");

        let then_jump = self.emit_jump(Op::JumpIfFalse(0xffff));
        // Will pop the condition if the branch is true
        self.emit_byte(Op::Pop);
        self.statement();

        self.skip_new_lines();
        self.expect_and_skip(TokenKind::RightBrace, "expect '}' after 'if' body");

        let else_jump = self.emit_jump(Op::Jump(0xffff));
        
        // Will pop the condition if the branch is false
        self.emit_byte(Op::Pop);

        self.patch_jump(then_jump);

        if self.is_at_and_skip(TokenKind::Else) {
            self.expect_and_skip(TokenKind::LeftBrace, "expect '{' before 'then' body");
            self.statement();
            self.skip_new_lines();
            self.expect_and_skip(TokenKind::RightBrace, "expect '}' after 'else' body");
        }

        self.patch_jump(else_jump);
    }

    fn while_statement(&mut self) {
        let loop_start = self.chunk.code.len() - 1;

        self.expression();
        self.expect_and_skip(TokenKind::LeftBrace, "expect '{' before 'while' body");

        let exit_jump = self.emit_jump(Op::JumpIfFalse(0xffff));
        self.emit_byte(Op::Pop);

        self.statement();
        self.emit_loop(loop_start);

        self.patch_jump(exit_jump);
        self.emit_byte(Op::Pop);
    }

    pub(super) fn and(&mut self, _: bool) {
        let end_jump = self.emit_jump(Op::JumpIfFalse(0xffff));
        self.emit_byte(Op::Pop);

        self.parse_precedence(Precedence::And);
        self.patch_jump(end_jump);
    }

    // TODO: Implement real JUMP_IF_TRUE Op, this is not clear and less efficient
    pub(super) fn or(&mut self, _: bool) {
        let else_jump = self.emit_jump(Op::JumpIfFalse(0xffff));
        let end_jump = self.emit_jump(Op::Jump(0xffff));

        self.patch_jump(else_jump);
        self.emit_byte(Op::Pop);

        self.parse_precedence(Precedence::Or);
        self.patch_jump(end_jump);
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
                self.error("expect expression");
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
        let id = self.resolve_local();

        let (set_op, get_op) = match id {
            -1 => {
                let id = self.identifier_constant();
                (Op::SetGlobal(id as u8), Op::GetGlobal(id as u8))
            },
            _ => (Op::SetLocal(id as u8), Op::GetLocal(id as u8))
        };

        if self.is_at(TokenKind::Equal) && can_assign {
            self.expression();
            self.emit_byte(set_op);
        } else {
            self.emit_byte(get_op);
        }
    }

    fn resolve_local(&mut self) -> i16 {
        for (idx, local) in self.scope.locals.iter().enumerate().rev() {
            if local.name == self.previous.lexeme {
                if local.depth == -1 {
                    self.error("can't use local variable in its own initializer");
                }

                return idx as i16
            }
        }

        return -1
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

    pub(super) fn emit_byte(&mut self, byte: Op) {
        self.chunk.write(byte, self.previous.line);
    }

    fn emit_bytes(&mut self, byte1: Op, byte2: Op) {
        self.emit_byte(byte1);
        self.emit_byte(byte2);
    }

    fn emit_byte_prev_line(&mut self, byte: Op) {
        let line = if self.previous.line == 0 { 0 } else { self.previous.line - 1 };
        self.chunk.write(byte, line);
    }

    fn emit_jump(&mut self, jump: Op) -> usize {
        self.emit_byte(jump);

        self.chunk.code.len() - 1
    }

    fn emit_loop(&mut self, start: usize) {
        let offset = self.chunk.code.len() - 1 - start;

        let offset = match u16::try_from(offset) {
            Ok(o) => o,
            Err(_) => {
                self.error("loop body is too large");
                0xffff
            }
        };

        self.emit_byte(Op::Loop(offset));
    }

    fn patch_jump(&mut self, jump_idx: usize) {
        let offset = self.chunk.code.len() - 1 - jump_idx;

        if offset > 0xffff {
            self.error("to much code to jump over");
        }
        
        match &mut self.chunk.code[jump_idx] {
            Op::JumpIfFalse(i)
            | Op::Jump(i) => *i = offset as u16,
            other => {
                unreachable!("Found: {:?}", other)
            }
        }
    }
}
