use crate::token::*;
use std::collections::HashMap;
use thiserror::Error;

#[derive(Debug, Error)]
pub enum ScanError {
    #[error("Scan error: Unexpected character '{ch}' at line {line}, column {col}")]
    UnexpectedCharacter { ch: char, line: usize, col: usize },

    #[error("Scan error: Unterminated string at line {line}")]
    UnterminatedString { line: usize },
}

impl ScanError {
    pub fn line(&self) -> usize {
        match self {
            ScanError::UnexpectedCharacter { line, .. } => *line,
            ScanError::UnterminatedString { line } => *line,
        }
    }
}

#[derive(Debug, Error)]
#[error("scan failed with {0} error(s)")]
pub struct ScanErrors(pub usize, pub Vec<ScanError>);

pub fn tokenize(source: &str) -> Result<Vec<Token>, ScanErrors> {
    let mut scanner = Scanner::new(source);
    scanner.scan_tokens()
}

struct Scanner {
    chars: Vec<char>,
    tokens: Vec<Token>,
    start: usize,
    current: usize,
    line: usize,
    col: usize,
    errors: Vec<ScanError>,
}

impl Scanner {
    fn new(source: &str) -> Self {
        Self {
            chars: source.chars().collect(),
            tokens: Vec::new(),
            start: 0,
            current: 0,
            line: 1,
            col: 0,
            errors: Vec::new(),
        }
    }

    fn scan_tokens(&mut self) -> Result<Vec<Token>, ScanErrors> {
        while !self.is_at_end() {
            self.start = self.current;
            self.scan_token();
        }

        self.tokens
            .push(Token::new(TokenKind::Eof, "".to_string(), self.line));

        if self.errors.is_empty() {
            Ok(std::mem::take(&mut self.tokens))
        } else {
            Err(ScanErrors(
                self.errors.len(),
                std::mem::take(&mut self.errors),
            ))
        }
    }

    fn is_at_end(&self) -> bool {
        self.current >= self.chars.len()
    }

    fn advance(&mut self) -> char {
        let ch = self.chars.get(self.current).copied().unwrap_or('\0');
        if ch != '\0' {
            self.current += 1;
            self.col += 1;
        }
        ch
    }

    fn peek(&self) -> char {
        self.chars.get(self.current).copied().unwrap_or('\0')
    }

    fn peek_next(&self) -> char {
        self.chars.get(self.current + 1).copied().unwrap_or('\0')
    }

    fn is_alpha(&self, c: char) -> bool {
        c.is_ascii_alphabetic() || c == '_'
    }

    fn is_alpha_numeric(&self, c: char) -> bool {
        self.is_alpha(c) || c.is_ascii_digit()
    }

    fn match_char(&mut self, expected: char) -> bool {
        if self.is_at_end() || self.peek() != expected {
            false
        } else {
            self.current += 1;
            self.col += 1;
            true
        }
    }

    fn add_token(&mut self, kind: TokenKind) {
        let lexeme: String = self.chars[self.start..self.current].iter().collect();
        let col_start = self.col.saturating_sub(lexeme.chars().count());
        let span = Span::new(self.start, self.current, self.line, col_start, self.col);
        self.tokens.push(Token::with_span(kind, lexeme, span));
    }

    fn scan_token(&mut self) {
        let c = self.advance();

        match c {
            // Single-character tokens
            '(' => self.add_token(TokenKind::LeftParen),
            ')' => self.add_token(TokenKind::RightParen),
            '{' => self.add_token(TokenKind::LeftBrace),
            '}' => self.add_token(TokenKind::RightBrace),
            ',' => self.add_token(TokenKind::Comma),
            '.' => self.add_token(TokenKind::Dot),
            '-' => self.add_token(TokenKind::Minus),
            '+' => self.add_token(TokenKind::Plus),
            ';' => self.add_token(TokenKind::Semicolon),
            '*' => self.add_token(TokenKind::Star),

            // Potentially two-character tokens
            '!' => {
                let token = if self.match_char('=') {
                    TokenKind::BangEqual
                } else {
                    TokenKind::Bang
                };
                self.add_token(token);
            }
            '=' => {
                let token = if self.match_char('=') {
                    TokenKind::EqualEqual
                } else {
                    TokenKind::Equal
                };
                self.add_token(token);
            }
            '>' => {
                let token = if self.match_char('=') {
                    TokenKind::GreaterEqual
                } else {
                    TokenKind::Greater
                };
                self.add_token(token);
            }
            '<' => {
                let token = if self.match_char('=') {
                    TokenKind::LessEqual
                } else {
                    TokenKind::Less
                };
                self.add_token(token);
            }

            // Comments and division
            '/' => {
                if self.match_char('/') {
                    while self.peek() != '\n' && !self.is_at_end() {
                        self.advance();
                    }
                } else {
                    self.add_token(TokenKind::Slash);
                }
            }

            // Whitespace
            ' ' | '\r' | '\t' => {}
            '\n' => {
                self.line += 1;
                self.col = 0;
            }

            // Literals
            '"' => self.scan_string(),
            c if c.is_ascii_digit() => self.scan_number(),
            c if self.is_alpha(c) => self.identifier(),

            // Unexpected character
            _ => self.errors.push(ScanError::UnexpectedCharacter {
                ch: c,
                line: self.line,
                col: self.col.saturating_sub(1),
            }),
        }
    }

    fn scan_string(&mut self) {
        while self.peek() != '"' && !self.is_at_end() {
            if self.peek() == '\n' {
                self.line += 1;
            }
            self.advance();
        }

        if self.is_at_end() {
            self.errors
                .push(ScanError::UnterminatedString { line: self.line });
            return;
        }

        self.advance();
        self.add_token(TokenKind::String);
    }

    fn scan_number(&mut self) {
        while self.peek().is_ascii_digit() {
            self.advance();
        }

        if self.peek() == '.' && self.peek_next().is_ascii_digit() {
            self.advance();

            while self.peek().is_ascii_digit() {
                self.advance();
            }
        }

        self.add_token(TokenKind::Number);
    }

    fn identifier(&mut self) {
        while self.is_alpha_numeric(self.peek()) {
            self.advance();
        }

        let text: String = self.chars[self.start..self.current].iter().collect();
        let token_type = Self::keywords()
            .get(text.as_str())
            .copied()
            .unwrap_or(TokenKind::Identifier);
        self.add_token(token_type);
    }

    fn keywords() -> &'static HashMap<&'static str, TokenKind> {
        use std::sync::OnceLock;
        static KEYWORDS: OnceLock<HashMap<&'static str, TokenKind>> = OnceLock::new();

        KEYWORDS.get_or_init(|| {
            let mut keywords = HashMap::new();
            keywords.insert("and", TokenKind::And);
            keywords.insert("class", TokenKind::Class);
            keywords.insert("else", TokenKind::Else);
            keywords.insert("false", TokenKind::False);
            keywords.insert("for", TokenKind::For);
            keywords.insert("fun", TokenKind::Fun);
            keywords.insert("if", TokenKind::If);
            keywords.insert("nil", TokenKind::Nil);
            keywords.insert("or", TokenKind::Or);
            keywords.insert("print", TokenKind::Print);
            keywords.insert("return", TokenKind::Return);
            keywords.insert("super", TokenKind::Super);
            keywords.insert("this", TokenKind::This);
            keywords.insert("true", TokenKind::True);
            keywords.insert("var", TokenKind::Var);
            keywords.insert("while", TokenKind::While);
            keywords
        })
    }
}
