use TokenKind::*;

#[derive(Debug, Default, PartialEq, Clone, Copy)]
pub enum TokenKind {
    // Single character
    LeftParen,
    RightParen,
    LeftBrace,
    RightBrace,
    Comma,
    Dot,
    Colon,
    Minus,
    Plus,
    Slash,
    Star,

    // One or two characters
    Bang,
    BangEqual,
    Equal,
    EqualEqual,
    Greater,
    GreaterEqual,
    Less,
    LessEqual,

    // Literals
    Identifier,
    String,
    Int,
    Float,

    // Keywords
    Struct,
    Fn,
    SelfKw,
    Var,
    Return,
    If,
    Else,
    And,
    Or,
    Null,
    Print,
    For,
    In,
    While,
    True,
    False,

    NewLine,
    Error,
    #[default]
    Eof,
}

#[derive(Default)]
pub struct Token<'src> {
    pub kind: TokenKind,
    pub line: usize,
    pub lexeme: &'src str,
}

pub struct Lexer<'src> {
    code: &'src str,
    bytes: &'src [u8],
    start: usize,
    current: usize,
    line: usize,
}

impl<'src> Lexer<'src> {
    pub fn new(code: &'src str) -> Self {
        Self {
            code,
            bytes: code.as_bytes(),
            start: 0,
            current: 0,
            line: 0,
        }
    }

    pub fn lex(&mut self) -> Token<'src> {
        self.skip_white_space();

        self.start = self.current;

        if self.eof() {
            return self.new_token(Eof);
        }

        match self.advance() {
            b'(' => self.new_token(LeftParen),
            b')' => self.new_token(RightParen),
            b'{' => self.new_token(LeftBrace),
            b'}' => self.new_token(RightBrace),
            b',' => self.new_token(Comma),
            b'.' => self.new_token(Dot),
            b':' => self.new_token(Colon),
            b'-' => self.new_token(Minus),
            b'+' => self.new_token(Plus),
            b'*' => self.new_token(Star),
            b'/' => self.new_token(Slash),
            b'\n' => {
                self.line += 1;
                self.new_token(NewLine)
            }
            b'!' => {
                let kind = if self.is_at(b'=') { BangEqual } else { Bang };
                self.new_token(kind)
            }
            b'=' => {
                let kind = if self.is_at(b'=') { EqualEqual } else { Equal };
                self.new_token(kind)
            }
            b'<' => {
                let kind = if self.is_at(b'=') { LessEqual } else { Less };
                self.new_token(kind)
            }
            b'>' => {
                let kind = if self.is_at(b'=') {
                    GreaterEqual
                } else {
                    Greater
                };
                self.new_token(kind)
            }
            b'"' => self.string(),
            tk => {
                if is_alpha(tk) {
                    return self.identifier();
                }

                if tk.is_ascii_digit() {
                    return self.number();
                }

                self.error_token("Unexpected token")
            }
        }
    }

    fn string(&mut self) -> Token<'src> {
        while !self.eof() && self.peek() != b'"' {
            if self.peek() == b'\n' {
                self.line += 1;
            }

            self.advance();
        }

        if self.eof() {
            return self.error_token("unterminated string");
        }

        // Closing quote
        self.advance();

        self.new_token(String)
    }

    fn identifier(&mut self) -> Token<'src> {
        while is_alpha_numeric(self.peek()) {
            self.advance();
        }

        self.new_token(self.identifier_type())
    }

    fn identifier_type(&self) -> TokenKind {
        match self.bytes[self.start] {
            b's' => {
                if self.current - self.start > 1 {
                    return match self.bytes[self.start + 1] {
                        b't' => self.check_keyword(2, 4, b"ruct", Struct),
                        b'e' => self.check_keyword(2, 2, b"lf", SelfKw),
                        _ => Identifier,
                    };
                }

                Identifier
            }
            b'f' => {
                if self.current - self.start > 1 {
                    return match self.bytes[self.start + 1] {
                        b'a' => self.check_keyword(2, 3, b"lse", False),
                        b'o' => self.check_keyword(2, 1, b"r", For),
                        b'n' => self.check_keyword(2, 0, b"", Fn),
                        _ => Identifier,
                    };
                }

                Identifier
            }
            b'i' => {
                if self.current - self.start > 1 {
                    return match self.bytes[self.start + 1] {
                        b'f' => self.check_keyword(2, 0, b"", If),
                        b'n' => self.check_keyword(2, 0, b"", In),
                        _ => Identifier,
                    };
                }

                Identifier
            }
            b'r' => self.check_keyword(1, 5, b"eturn", Return),
            b'v' => self.check_keyword(1, 2, b"ar", Var),
            b'e' => self.check_keyword(1, 3, b"lse", Else),
            b'a' => self.check_keyword(1, 2, b"nd", And),
            b'o' => self.check_keyword(1, 1, b"r", Or),
            b'n' => self.check_keyword(1, 3, b"ull", Null),
            b'p' => self.check_keyword(1, 4, b"rint", Print),
            b'w' => self.check_keyword(1, 4, b"hile", While),
            b't' => self.check_keyword(1, 3, b"rue", True),
            _ => Identifier,
        }
    }

    fn check_keyword(
        &self,
        start: usize,
        length: usize,
        rest: &[u8],
        kind: TokenKind,
    ) -> TokenKind {
        if self.current - self.start == start + length
            && &self.bytes[self.start + start..self.current] == rest
        {
            return kind
        }

        Identifier
    }

    fn number(&mut self) -> Token<'src> {
        while self.peek().is_ascii_digit() {
            self.advance();
        }

        if self.peek() == b'.' && self.peek_next().is_ascii_digit() {
            self.advance();

            while self.peek().is_ascii_digit() {
                self.advance();
            }

            return self.new_token(Float);
        }

        self.new_token(Int)
    }

    fn new_token(&self, kind: TokenKind) -> Token<'src> {
        Token {
            kind,
            line: self.line,
            lexeme: &self.code[self.start..self.current],
        }
    }

    fn error_token(&self, msg: &'static str) -> Token<'src> {
        Token {
            kind: Error,
            line: self.line,
            lexeme: msg,
        }
    }

    fn advance(&mut self) -> u8 {
        self.current += 1;
        self.bytes[self.current - 1]
    }

    fn is_at(&mut self, expected: u8) -> bool {
        if self.eof() {
            return false;
        }
        if self.bytes[self.current] != expected {
            return false;
        }

        self.current += 1;
        true
    }

    fn peek(&self) -> u8 {
        if self.eof() {
            return b'\0';
        }

        self.bytes[self.current]
    }

    fn peek_next(&self) -> u8 {
        if self.current + 1 == self.code.len() {
            return b'\0';
        }

        self.bytes[self.current + 1]
    }

    fn skip_white_space(&mut self) {
        while !self.eof() {
            match self.peek() {
                b' ' | b'\r' | b'\t' => {
                    self.advance();
                }
                b'/' => {
                    if self.peek_next() == b'/' {
                        self.skip_comment();
                    }

                    break;
                }
                _ => return,
            }
        }
    }

    fn skip_comment(&mut self) {
        while self.peek() != b'\n' && !self.eof() {
            self.advance();
        }
    }

    fn eof(&self) -> bool {
        self.current >= self.code.len()
    }
}

fn is_alpha(byte: u8) -> bool {
    byte.is_ascii_alphabetic() || byte == b'_'
}

fn is_alpha_numeric(byte: u8) -> bool {
    byte.is_ascii_alphanumeric() || byte == b'_'
}
