#[derive(Clone, Debug, PartialEq)]
pub struct Token {
    pub kind: TokenKind,
    pub lexeme: String,
    pub literal: Option<LiteralKind>,
}

impl Token {
    pub fn new(kind: TokenKind, lexeme: String, literal: Option<LiteralKind>) -> Token {
        Token {
            kind,
            lexeme,
            literal,
        }
    }
    pub fn to_string(&self) -> String {
        match &self.literal {
            Some(literal) => format!("{} \"{}\" {}", self.kind, self.lexeme, literal),
            None => {
                if self.kind == TokenKind::Eof {
                    format!("{}", self.kind)
                } else {
                    format!("{} \"{}\"", self.kind, self.lexeme)
                }
            }
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum TokenKind {
    LeftParen,
    RightParen,
    LeftBrace,
    RightBrace,
    Comma,
    Dot,
    Minus,
    Plus,
    Semicolon,
    Slash,
    Star,
    Bang,
    BangEqual,
    Equal,
    EqualEqual,
    Greater,
    GreaterEqual,
    Less,
    LessEqual,
    Identifier,
    String,
    Number,
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
        write!(f, "{}", self.lexeme)
    }
}

impl std::fmt::Display for TokenKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            TokenKind::LeftParen => write!(f, "LeftParen"),
            TokenKind::RightParen => write!(f, "RightParen"),
            TokenKind::LeftBrace => write!(f, "LeftBrace"),
            TokenKind::RightBrace => write!(f, "RightBrace"),
            TokenKind::Comma => write!(f, "Comma"),
            TokenKind::Dot => write!(f, "Dot"),
            TokenKind::Minus => write!(f, "Minus"),
            TokenKind::Plus => write!(f, "Plus"),
            TokenKind::Semicolon => write!(f, "Semicolon"),
            TokenKind::Slash => write!(f, "Slash"),
            TokenKind::Star => write!(f, "Star"),
            TokenKind::Bang => write!(f, "Bang"),
            TokenKind::BangEqual => write!(f, "BangEqual"),
            TokenKind::Equal => write!(f, "Equal"),
            TokenKind::EqualEqual => write!(f, "EqualEqual"),
            TokenKind::Greater => write!(f, "Greater"),
            TokenKind::GreaterEqual => write!(f, "GreaterEqual"),
            TokenKind::Less => write!(f, "Less"),
            TokenKind::LessEqual => write!(f, "LessEqual"),
            TokenKind::Identifier => write!(f, "Identifier"),
            TokenKind::String => write!(f, "String"),
            TokenKind::Number => write!(f, "Number"),
            TokenKind::And => write!(f, "And"),
            TokenKind::Class => write!(f, "Class"),
            TokenKind::Else => write!(f, "Else"),
            TokenKind::False => write!(f, "False"),
            TokenKind::Fun => write!(f, "fun"),
            TokenKind::For => write!(f, "For"),
            TokenKind::If => write!(f, "If"),
            TokenKind::Nil => write!(f, "Nil"),
            TokenKind::Or => write!(f, "Or"),
            TokenKind::Print => write!(f, "Print"),
            TokenKind::Return => write!(f, "Return"),
            TokenKind::Super => write!(f, "Super"),
            TokenKind::This => write!(f, "This"),
            TokenKind::True => write!(f, "True"),
            TokenKind::Var => write!(f, "Var"),
            TokenKind::While => write!(f, "While"),
            TokenKind::Eof => write!(f, "EOF"),
        }
    }
}

impl std::fmt::Display for LiteralKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            LiteralKind::Int => write!(f, "int"),
            LiteralKind::Decimal => write!(f, "decimal"),
            LiteralKind::String => write!(f, "string"),
        }
    }
}
