#[derive(Clone, Debug, PartialEq)]
pub struct Token {
    pub kind: TokenKind,
    pub lexeme: String,
    pub literal: Option<LiteralKind>,
    pub line: usize,
}

impl Token {
    pub fn new(kind: TokenKind, lexeme: String, literal: Option<LiteralKind>, line: usize) -> Self {
        Self {
            kind,
            lexeme,
            literal,
            line,
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum TokenKind {
    Literal { kind: LiteralKind },

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

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum LiteralKind {
    Int,
    Decimal,
    String,
}

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
            Self::Literal { kind } => return write!(f, "Literal ({})", kind),

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

impl std::fmt::Display for LiteralKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let s = match self {
            Self::Int => "int",
            Self::Decimal => "decimal",
            Self::String => "string",
        };
        write!(f, "{}", s)
    }
}
