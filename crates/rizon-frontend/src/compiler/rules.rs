use super::{Compiler, Precedence};

use Precedence as P;


pub(super) type CompilerFn<'src> = fn(&mut Compiler<'src>, bool);

pub(super) struct Rule<'src> {
    pub prefix: Option<CompilerFn<'src>>,
    pub infix: Option<CompilerFn<'src>>,
    pub precedence: Precedence,
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

pub(super) type Rules<'src> = [Rule<'src>; 42];

pub(super) fn make_rules<'src>() -> Rules<'src> {
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
        Rule::new(Some(Compiler::variable), None, P::None),                // Identifier
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
        Rule::new(None, Some(Compiler::and), P::And),                      // And
        Rule::new(None, Some(Compiler::or), P::Or),                        // Or
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