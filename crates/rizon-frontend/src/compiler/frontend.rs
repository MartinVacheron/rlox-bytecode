use super::ByteCodeGen;
use crate::lexer::TokenKind;


pub enum ErrTokenPos {
    Previous,
    Current,
}

impl<'src> ByteCodeGen<'src> {
    pub(super) fn advance(&mut self) {
        std::mem::swap(&mut self.previous, &mut self.current);

        loop {
            self.current = self.lexer.lex();

            if !self.check(TokenKind::Error) {
                break;
            }

            self.error_at_current(&self.current.lexeme);
        }
    }

    pub(super) fn skip_new_lines(&mut self) {
        while self.check(TokenKind::NewLine) {
            self.advance();
        }
    }

    pub(super) fn is_at(&mut self, kind: TokenKind) -> bool {
        if !self.check(kind) {
            return false;
        }

        self.advance();
        true
    }

    pub(super) fn is_at_and_skip(&mut self, kind: TokenKind) -> bool {
        let res = self.is_at(kind);
        self.skip_new_lines();
        res
    }

    pub(super) fn expect(&mut self, kind: TokenKind, msg: &str) {
        if self.check(kind) {
            self.advance();
            return;
        }

        self.error_at_current(msg);
    }

    pub(super) fn expect_and_skip(&mut self, kind: TokenKind, msg: &str) {
        self.expect(kind, msg);
        self.skip_new_lines();
    }

    pub(super) fn expect_or_eof(&mut self, kind: TokenKind, msg: &str) {
        if self.check(kind) {
            self.advance();
            return;
        } else if self.check(TokenKind::Eof) {
            return
        }

        self.error_at_current(msg);
    }

    pub(super) fn check(&self, kind: TokenKind) -> bool {
        self.current.kind == kind
    }

    pub(super) fn eof(&self) -> bool {
        self.current.kind == TokenKind::Eof
    }

    pub(super) fn error_at_current(&mut self, msg: &str) {
        self.error_at(ErrTokenPos::Current, msg);
    }

    pub(super) fn error(&mut self, msg: &str) {
        self.error_at(ErrTokenPos::Previous, msg);
    }

    pub(super) fn error_at(&mut self, token_pos: ErrTokenPos, msg: &str) {
        if self.panic_mode {
            return;
        }

        let token = match token_pos {
            ErrTokenPos::Previous => &self.previous,
            ErrTokenPos::Current => &self.current,
        };

        print!("[line {}] Error", token.line);

        match token.kind {
            TokenKind::Eof => print!(" at end"),
            TokenKind::Error => {}
            _ => print!(" at '{}'", token.lexeme),
        }

        println!(": {}", msg);

        self.had_error = true;
        self.panic_mode = true;
    }

    pub(super) fn synchronize(&mut self) {
        self.panic_mode = false;

        while !self.check(TokenKind::Eof) {
            if self.previous.kind == TokenKind::NewLine {
                return;
            }

            if matches!(
                self.current.kind,
                TokenKind::Struct
                    | TokenKind::Fn
                    | TokenKind::Var
                    | TokenKind::For
                    | TokenKind::If
                    | TokenKind::While
                    | TokenKind::Print
                    | TokenKind::Return
            ) {
                return;
            }

            self.advance();
        }
    }
}
