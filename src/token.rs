#[derive(Clone, Debug, PartialEq)]
pub struct Token {
    pub kind: TokenKind,
    pub lexeme: String,
    pub line: usize,
}

impl Token {
    pub fn new(kind: TokenKind, lexeme: String, line: usize) -> Self {
        Self { kind, lexeme, line }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum TokenKind {
    // Literals and identifiers
    Number,
    String,
    Identifier,

    // Single-character tokens (punctuation)
    LeftParen,  // (
    RightParen, // )
    LeftBrace,  // {
    RightBrace, // }
    Comma,      // ,
    Dot,        // .
    Semicolon,  // ;

    // Operators
    Minus,   // -
    Plus,    // +
    Slash,   // /
    Star,    // *
    Bang,    // !
    Equal,   // =
    Greater, // >
    Less,    // <

    // Two-character operators
    BangEqual,    // !=
    EqualEqual,   // ==
    GreaterEqual, // >=
    LessEqual,    // <=

    // Keywords
    And,
    Class,
    Else,
    False,
    Fun,
    For,
    If,
    Nil,
    Or,
    Print,
    Return,
    Super,
    This,
    True,
    Var,
    While,

    // Special
    Eof,
}

// NOTE: LiteralKind removed; literals are represented directly via TokenKind

impl std::fmt::Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if self.kind == TokenKind::Eof {
            write!(f, "{:<12} {:>10} [line {}]", self.kind, "", self.line)
        } else {
            write!(
                f,
                "{:<12} {:>10} [line {}]",
                self.kind,
                format!("\"{}\"", self.lexeme),
                self.line
            )
        }
    }
}

impl std::fmt::Display for TokenKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let s = match self {
            // Literals and identifiers
            Self::Number => "Number",
            Self::String => "String",
            Self::Identifier => "Identifier",

            // Punctuation
            Self::LeftParen => "LeftParen",
            Self::RightParen => "RightParen",
            Self::LeftBrace => "LeftBrace",
            Self::RightBrace => "RightBrace",
            Self::Comma => "Comma",
            Self::Dot => "Dot",
            Self::Semicolon => "Semicolon",

            // Operators
            Self::Minus => "Minus",
            Self::Plus => "Plus",
            Self::Slash => "Slash",
            Self::Star => "Star",
            Self::Bang => "Bang",
            Self::Equal => "Equal",
            Self::Greater => "Greater",
            Self::Less => "Less",
            Self::BangEqual => "BangEqual",
            Self::EqualEqual => "EqualEqual",
            Self::GreaterEqual => "GreaterEqual",
            Self::LessEqual => "LessEqual",

            // Keywords
            Self::And => "And",
            Self::Class => "Class",
            Self::Else => "Else",
            Self::False => "False",
            Self::Fun => "fun",
            Self::For => "For",
            Self::If => "If",
            Self::Nil => "Nil",
            Self::Or => "Or",
            Self::Print => "Print",
            Self::Return => "Return",
            Self::Super => "Super",
            Self::This => "This",
            Self::True => "True",
            Self::Var => "Var",
            Self::While => "While",

            // Special
            Self::Eof => "EOF",
        };
        write!(f, "{}", s)
    }
}
