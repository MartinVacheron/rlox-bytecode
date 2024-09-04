use crate::{chunk::Op, lexer::TokenKind, value::Value};

use super::{rules::Rule, ByteCodeGen, FnType, Precedence, StructCompiler};

use Precedence as P;


impl<'src> ByteCodeGen<'src> {
    pub(super) fn declaration(&mut self) {
        self.skip_new_lines();

        if self.is_at(TokenKind::Var) {
            self.var_declaration();
        } else if self.is_at(TokenKind::Fn) {
            self.fn_declaration();
        } else if self.is_at(TokenKind::Struct) {
            self.struct_declaration();
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

        // End of one-line if for example
        if !self.check(TokenKind::RightBrace) {
            self.expect_or_eof(TokenKind::NewLine, "expected end of line after var declaration");
        }

        self.define_variable(id);
    }

    fn parse_variable(&mut self, msg: &str) -> u8 {
        self.expect(TokenKind::Identifier, msg);

        self.declare_variable();

        if self.compiler.scope.depth > 0 {
            return 0
        }

        self.identifier_constant()
    }

    fn declare_variable(&mut self) {
        if self.compiler.scope.depth == 0 {
            return
        }

        if self.compiler.scope.locals.len() > 0 {
            for i in (0..=self.compiler.scope.locals.len() - 1).rev() {
                if self.compiler.scope.locals[i].depth < self.compiler.scope.depth {
                    break
                }
    
                if self.compiler.scope.locals[i].name == self.previous.lexeme {
                    self.error("already declared variable in this scope");
                }
            }
        }

        self.add_local(&self.previous.lexeme);
    }

    fn identifier_constant(&mut self) -> u8 {
        let name = self.gc.intern(self.previous.lexeme.to_string());
        let name = Value::Str(name);
        self.make_constant(name)
    }

    fn define_variable(&mut self, id: u8) {
        if self.compiler.scope.depth > 0 {
            self.mark_initialized();
            return
        }

        self.emit_byte_prev_line(Op::DefineGlobal(id))
    }

    fn fn_declaration(&mut self) {
        let idx = self.parse_variable("expected function name");
        self.mark_initialized();
        self.function(FnType::Function);
        self.define_variable(idx);
    }

    fn function(&mut self, fn_type: FnType) {
        self.push_compiler(fn_type);
        self.begin_scope();

        self.expect_and_skip(TokenKind::LeftParen, "expected '(' after function name");

        if !self.is_at(TokenKind::RightParen) {
            loop {
                self.compiler.function.arity += 1;

                if self.compiler.function.arity > 255 {
                    self.error_at_current("can't have more than 255 parameters");
                }

                let idx = self.parse_variable("expect parameter name");
                self.define_variable(idx);

                if !self.is_at(TokenKind::Comma) || self.check(TokenKind::RightParen) {
                    break
                }
                self.skip_new_lines();
            }

            self.expect_and_skip(TokenKind::RightParen, "expected ')' after function parameters");
        }

        self.expect_and_skip(TokenKind::LeftBrace, "expected '{' before function body");

        self.block();

        let function = self.pop_compiler();
        let function = self.gc.alloc(function);
        let idx = self.make_constant(Value::Fn(function));
        self.emit_byte_prev_line(Op::Closure(idx));
    }

    fn struct_declaration(&mut self) {
        self.expect_and_skip(TokenKind::Identifier, "expect structure name after 'struct' keyword");
        let name_idx = self.identifier_constant();
        self.declare_variable();

        let old = self.current_struct.take();
        self.current_struct = Some(StructCompiler::new(old));

        self.emit_byte(Op::Struct(name_idx));
        // Make it available for self reference in its body
        self.define_variable(name_idx);

        // Loads the struct variable on the stack so that each Op::Method
        // is right next to it
        // The name is the previous token (used in identifier_constant)
        self.named_variable(false);
        self.expect_and_skip(TokenKind::LeftBrace, "expected '{' before structure body");

        if self.check(TokenKind::Fn) {
            while !self.check(TokenKind::RightBrace) && !self.check(TokenKind::Eof) {
                self.expect(TokenKind::Fn, "expect 'fn' keyword to declare methods");
                self.method();
                self.skip_new_lines();
            }
        }

        match self.current_struct.take() {
            Some(cur) => self.current_struct = cur.enclosing,
            None =>  self.current_struct = None
        }
        
        self.expect_and_skip(TokenKind::RightBrace, "expected '}' after structure body");
        self.emit_byte_prev_line(Op::Pop);
    }

    fn method(&mut self) {
        self.expect_and_skip(TokenKind::Identifier, "expect method name");
        let name_idx = self.identifier_constant();

        let fn_type = if self.previous.lexeme == "init" {
            FnType::Initializer
        } else {
            FnType::Method
        };

        self.function(fn_type);
        self.emit_byte(Op::Method(name_idx));
    }

    fn statement(&mut self) {
        if self.is_at(TokenKind::Print) {
            self.print_statement();
        } else if self.is_at_and_skip(TokenKind::LeftBrace) {
            self.begin_scope();
            self.block();
            self.end_scope();
        } else if self.is_at(TokenKind::Return) {
            self.return_statement();
        } else if self.is_at(TokenKind::If) {
            self.if_statement();
        } else if self.is_at(TokenKind::While) {
            self.while_statement();
        } else if self.is_at(TokenKind::For) {
            self.for_statement();
        } else {
            self.expression_statement();
        }

        self.skip_new_lines();
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

        self.expect_and_skip(TokenKind::RightBrace, "expect '}' after block");
    }

    fn return_statement(&mut self) {
        if self.compiler.fn_type == FnType::Script {
            self.error("can't return from top-level code");
        }

        if self.is_at(TokenKind::NewLine) {
            self.emit_return();
        } else {
            if self.compiler.fn_type == FnType::Initializer {
                self.error("can't return a value from a constructor");
            }

            self.expression();
            self.emit_byte(Op::Return);
        }
    }

    fn if_statement(&mut self) {
        self.expression();
        self.skip_new_lines();
        let then_jump = self.emit_jump(Op::JumpIfFalse(0xffff));
        // Will pop the condition if the branch is true
        self.emit_byte(Op::Pop);
        self.statement();

        let else_jump = self.emit_jump(Op::Jump(0xffff));
        
        self.patch_jump(then_jump);
        // Will pop the condition if the branch is false
        self.emit_byte(Op::Pop);

        if self.is_at_and_skip(TokenKind::Else) {
            self.statement();
        }

        self.patch_jump(else_jump);
    }

    fn while_statement(&mut self) {
        let loop_start = self.start_loop();

        self.expression();

        if !self.check(TokenKind::LeftBrace) {
            self.error("expect '{' before 'while' body");
            return
        }

        let exit_jump = self.emit_jump(Op::JumpIfFalse(0xffff));
        self.emit_byte(Op::Pop);

        self.statement();
        self.emit_loop(loop_start);

        self.patch_jump(exit_jump);
        self.emit_byte(Op::Pop);
    }

    fn for_statement(&mut self) {
        self.begin_scope();
        // Placeholder var
        let idx = self.parse_variable("expected placeholder variable");
        self.define_variable(idx);
        self.expect(TokenKind::In, "expected 'in' keyword after variable");
        
        self.expect(TokenKind::Int, "expected 'int' value to iterate on");
        self.int(false); // placeholder value. It takes the range int as value
        self.emit_byte(Op::CreateIter);
        // Because the VM pushes a Value::Iter on the stack too
        // it's to keep them aligned. Mark initialized is important for the depth
        self.add_local("__iter");
        self.mark_initialized();
        let iter_idx = self.compiler.scope.locals.len() - 1;

        let loop_start = self.start_loop();
        let exit_jump = self.emit_jump(Op::ForIter(iter_idx as u8, 0xffff));

        // The fact of doing a scope with statement allow to clean the whole loop before
        // starting the next round.
        self.statement();
        self.emit_loop(loop_start);

        self.patch_jump(exit_jump);
        self.end_scope();
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

    pub(super) fn call(&mut self, _: bool) {
        let args_count = self.args_list();

        self.emit_byte(Op::Call(args_count as u8));
    }

    pub(super) fn dot(&mut self, can_assign: bool) {
        self.expect(TokenKind::Identifier, "expect property name after '.'");
        let name_idx = self.identifier_constant();

        if can_assign && self.is_at(TokenKind::Equal) {
            self.expression();
            self.emit_byte(Op::SetProperty(name_idx));
        } else if self.is_at(TokenKind::LeftParen) {
            let args_count = self.args_list();
            self.emit_byte(Op::Invoke((name_idx, args_count as u8)));
        } else {
            self.emit_byte(Op::GetProperty(name_idx));
        }
    }

    fn args_list(&mut self) -> usize {
        let mut count = 0;

        if !self.is_at(TokenKind::RightParen) {
            loop {
                self.skip_new_lines();
                self.expression();

                if count == 255 {
                    self.error("can't have more than 255 arguments");
                }

                count += 1;

                if !self.is_at(TokenKind::Comma) || self.check(TokenKind::RightParen) {
                    break
                }
            }

            self.expect(TokenKind::RightParen, "expect ')' after arguments");
        }

        count
    }

    pub(super) fn float(&mut self, _: bool) {
        let val = self
            .previous
            .lexeme
            .parse::<f64>()
            .expect("Error parsing float");

        self.emit_constant(Value::Float(val));
    }

    pub(super) fn int(&mut self, _: bool) {
        let val = self
            .previous
            .lexeme
            .parse::<i64>()
            .expect("Error parsing int");

        self.emit_constant(Value::Int(val));
    }

    pub(super) fn string(&mut self, _: bool) {
        let str_len = self.previous.lexeme.len();
        let final_str = self.previous.lexeme[1..str_len-1].to_string();
        let string = self.gc.intern(final_str);
        self.emit_constant(Value::Str(string));
    }

    pub(super) fn variable(&mut self, can_assign: bool) {
        self.named_variable(can_assign);
    }

    pub(super) fn self_(&mut self, _: bool) {
        if self.current_struct.is_none() {
            self.error("can't use 'self' outside of a structure");
            return
        }

        self.variable(false);
    }

    fn named_variable(&mut self, can_assign: bool) {
        let idx = self.compiler.resolve_local(&self.previous.lexeme);

        let (set_op, get_op) = match idx {
            None => {
                match self.compiler.resolve_upvalue(&self.previous.lexeme) {
                    Some(upval_idx) => (Op::SetUpValue(upval_idx), Op::GetUpValue(upval_idx)),
                    None => {
                        let id = self.identifier_constant();
                        (Op::SetGlobal(id), Op::GetGlobal(id))
                    }
                }
            },
            Some(local_idx) => (Op::SetLocal(local_idx), Op::GetLocal(local_idx)),
        };

        if can_assign && self.is_at(TokenKind::Equal) {
            self.expression();
            self.emit_byte(set_op);
        } else {
            self.emit_byte(get_op);
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

    fn make_constant(&mut self, value: Value) -> u8 {
        let id = self.chunk_mut().write_constant(value);

        match u8::try_from(id) {
            Ok(id) => id,
            Err(_) => {
                self.error("Too many constants in one chunk");
                0
            }
        }
    }

    fn get_rule(&self, kind: TokenKind) -> &Rule<'src> {
        &self.rules[kind as usize]
    }

    fn emit_constant(&mut self, value: Value) {
        let id = self.make_constant(value);
        self.emit_byte(Op::Constant(id));
    }

    pub(super) fn emit_byte(&mut self, byte: Op) {
        let prev_line = self.previous.line;
        self.chunk_mut().write(byte, prev_line);
    }

    fn emit_bytes(&mut self, byte1: Op, byte2: Op) {
        self.emit_byte(byte1);
        self.emit_byte(byte2);
    }

    fn emit_byte_prev_line(&mut self, byte: Op) {
        let line = if self.previous.line == 0 { 0 } else { self.previous.line - 1 };
        self.chunk_mut().write(byte, line);
    }

    pub(super) fn emit_return(&mut self) {
        if self.compiler.fn_type == FnType::Initializer {
            // Slot 0 contains the 'self' variable
            self.emit_byte(Op::GetLocal(0));
        } else {
            self.emit_byte(Op::Null);
        }

        self.emit_byte(Op::Return);
    }

    fn emit_jump(&mut self, jump: Op) -> usize {
        self.emit_byte(jump);

        self.chunk_last_idx()
    }

    fn start_loop(&self) -> usize {
        self.chunk().code.len()
    }

    fn emit_loop(&mut self, start: usize) {
        // +1 because when we hit the Op::Loop, we are already passed it
        // we have to jump 1 more backward
        let offset = self.chunk().code.len() - start + 1;

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
        // This is the strict offset but as we are going to pop
        // the Op::Jump when erading it, we will land on the 
        // following instruction so it's ok
        let offset = self.chunk_last_idx() - jump_idx;

        if offset > 0xffff {
            self.error("to much code to jump over");
        }
        
        match &mut self.chunk_mut().code[jump_idx] {
            Op::JumpIfFalse(i)
            | Op::Jump(i)
            | Op::ForIter(_, i) => *i = offset as u16,
            other => {
                unreachable!("Found: {:?}", other)
            }
        }
    }
}
