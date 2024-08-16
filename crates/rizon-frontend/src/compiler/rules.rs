use super::{ByteCodeGen, Precedence};

use Precedence as P;


pub(super) type ByteCodeGenFn<'src> = fn(&mut ByteCodeGen<'src>, bool);

pub(super) struct Rule<'src> {
    pub prefix: Option<ByteCodeGenFn<'src>>,
    pub infix: Option<ByteCodeGenFn<'src>>,
    pub precedence: Precedence,
}

impl<'src> Rule<'src> {
    pub fn new(
        prefix: Option<ByteCodeGenFn<'src>>,
        infix: Option<ByteCodeGenFn<'src>>,
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
        Rule::new(Some(ByteCodeGen::grouping), Some(ByteCodeGen::call), P::Call),  // LeftParen
        Rule::new(None, None, P::None),                                            // RightParen
        Rule::new(None, None, P::None),                                            // LeftBrace
        Rule::new(None, None, P::None),                                            // RightBrace
        Rule::new(None, None, P::None),                                            // Comma
        Rule::new(None, None, P::None),                                            // Dot
        Rule::new(None, None, P::None),                                            // Colon
        Rule::new(Some(ByteCodeGen::unary), Some(ByteCodeGen::binary), P::Term),   // Minus
        Rule::new(None, Some(ByteCodeGen::binary), P::Term),                       // Plus
        Rule::new(None, Some(ByteCodeGen::binary), P::Factor),                     // Slash
        Rule::new(None, Some(ByteCodeGen::binary), P::Factor),                     // Star
        Rule::new(Some(ByteCodeGen::unary), None, P::None),                        // Bang
        Rule::new(None, Some(ByteCodeGen::binary), P::Equality),                   // BangEqual
        Rule::new(None, None, P::None),                                            // Equal
        Rule::new(None, Some(ByteCodeGen::binary), P::Equality),                   // EqualEqual
        Rule::new(None, Some(ByteCodeGen::binary), P::Comparison),                 // Greater
        Rule::new(None, Some(ByteCodeGen::binary), P::Comparison),                 // GreaterEqual
        Rule::new(None, Some(ByteCodeGen::binary), P::Comparison),                 // Less
        Rule::new(None, Some(ByteCodeGen::binary), P::Comparison),                 // LessEqual
        Rule::new(Some(ByteCodeGen::variable), None, P::None),                     // Identifier
        Rule::new(Some(ByteCodeGen::string), None, P::None),                       // String
        Rule::new(Some(ByteCodeGen::int), None, P::None),                          // Int
        Rule::new(Some(ByteCodeGen::float), None, P::None),                        // Float
        Rule::new(None, None, P::None),                                            // Struct
        Rule::new(None, None, P::None),                                            // Fn
        Rule::new(None, None, P::None),                                            // SelfKw
        Rule::new(None, None, P::None),                                            // Var
        Rule::new(None, None, P::None),                                            // Return
        Rule::new(None, None, P::None),                                            // If
        Rule::new(None, None, P::None),                                            // Else
        Rule::new(None, Some(ByteCodeGen::and), P::And),                           // And
        Rule::new(None, Some(ByteCodeGen::or), P::Or),                             // Or
        Rule::new(Some(ByteCodeGen::literal), None, P::None),                      // Null
        Rule::new(None, None, P::None),                                            // Print
        Rule::new(None, None, P::None),                                            // For
        Rule::new(None, None, P::None),                                            // In
        Rule::new(None, None, P::None),                                            // While
        Rule::new(Some(ByteCodeGen::literal), None, P::None),                      // True
        Rule::new(Some(ByteCodeGen::literal), None, P::None),                      // False
        Rule::new(None, None, P::None),                                            // NewLine
        Rule::new(None, None, P::None),                                            // Error
        Rule::new(None, None, P::None),                                            // Eof
    ]
}