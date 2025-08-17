#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct Span {
    pub start: usize,
    pub end: usize,
    pub line: usize,
    pub col_start: usize,
    pub col_end: usize,
}

impl Span {
    pub fn new(start: usize, end: usize, line: usize, col_start: usize, col_end: usize) -> Self {
        Self {
            start,
            end,
            line,
            col_start,
            col_end,
        }
    }

    pub fn line_bounds(source: &str, line: usize) -> (usize, usize) {
        let mut start = 0usize;
        let mut current_line = 1usize;
        for (idx, ch) in source.char_indices() {
            if current_line == line {
                break;
            }
            if ch == '\n' {
                current_line += 1;
                start = idx + 1;
            }
        }
        let mut end = start;
        for (idx, ch) in source[start..].char_indices() {
            if ch == '\n' {
                end = start + idx;
                break;
            }
            end = start + idx + 1;
        }
        (start, end)
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Token {
    pub kind: TokenKind,
    pub lexeme: String,
    pub span: Span,
}

impl Token {
    pub fn new(kind: TokenKind, lexeme: String, line: usize) -> Self {
        Self {
            kind,
            lexeme,
            span: Span::new(0, 0, line, 0, 0),
        }
    }

    pub fn with_span(kind: TokenKind, lexeme: String, span: Span) -> Self {
        Self { kind, lexeme, span }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum TokenKind {
    // Literals
    Number,
    String,
    Identifier,

    // Single-character
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

    // Two-character
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
    Return,
    Super,
    This,
    True,
    Var,
    While,

    // Special
    Eof,
}

impl std::fmt::Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if self.kind == TokenKind::Eof {
            write!(f, "{:<12} {:>10} [line {}]", self.kind, "", self.span.line)
        } else {
            write!(
                f,
                "{:<12} {:>10} [line {}]",
                self.kind,
                format!("\"{}\"", self.lexeme),
                self.span.line
            )
        }
    }
}

impl std::fmt::Display for TokenKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let s = match self {
            Self::Number => "Number",
            Self::String => "String",
            Self::Identifier => "Identifier",

            // Single-character
            Self::LeftParen => "LeftParen",
            Self::RightParen => "RightParen",
            Self::LeftBrace => "LeftBrace",
            Self::RightBrace => "RightBrace",
            Self::Comma => "Comma",
            Self::Dot => "Dot",
            Self::Semicolon => "Semicolon",

            // Two-character
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
            Self::Fun => "Fun",
            Self::For => "For",
            Self::If => "If",
            Self::Nil => "Nil",
            Self::Or => "Or",
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
