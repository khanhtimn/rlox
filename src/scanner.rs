use crate::token::*;
use std::collections::HashMap;
use thiserror::Error;

#[derive(Debug, Error)]
pub enum ScanError {
    #[error("Unexpected character '{ch}'")]
    UnexpectedCharacter { ch: char, span: Span },

    #[error("Unterminated string")]
    UnterminatedString { span: Span },
}

impl ScanError {
    pub fn span(&self) -> Span {
        match self {
            ScanError::UnexpectedCharacter { span, .. } => *span,
            ScanError::UnterminatedString { span } => *span,
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

struct Scanner<'a> {
    source: &'a str,
    tokens: Vec<Token>,
    start_byte: usize,
    current_byte: usize,
    start_col: usize,
    line: usize,
    col: usize,
    errors: Vec<ScanError>,
}

impl<'a> Scanner<'a> {
    fn new(source: &'a str) -> Self {
        Self {
            source,
            tokens: Vec::new(),
            start_byte: 0,
            current_byte: 0,
            start_col: 0,
            line: 1,
            col: 0,
            errors: Vec::new(),
        }
    }

    fn scan_tokens(&mut self) -> Result<Vec<Token>, ScanErrors> {
        while !self.is_at_end() {
            self.start_byte = self.current_byte;
            self.start_col = self.col;
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
        self.current_byte >= self.source.len()
    }

    fn next_char(&self) -> Option<(char, usize)> {
        self.source[self.current_byte..]
            .chars()
            .next()
            .map(|ch| (ch, ch.len_utf8()))
    }

    fn advance(&mut self) -> char {
        if let Some((ch, len)) = self.next_char() {
            self.current_byte += len;
            self.col += 1;
            ch
        } else {
            '\0'
        }
    }

    fn peek(&self) -> char {
        self.source[self.current_byte..]
            .chars()
            .next()
            .unwrap_or('\0')
    }

    fn peek_next(&self) -> char {
        let mut it = self.source[self.current_byte..].chars();
        it.next();
        it.next().unwrap_or('\0')
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
            self.advance();
            true
        }
    }

    fn add_token(&mut self, kind: TokenKind) {
        let start_byte = self.start_byte;
        let end_byte = self.current_byte;
        let lexeme = self.source[start_byte..end_byte].to_string();
        let span = Span::new(start_byte, end_byte, self.line, self.start_col, self.col);
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
            _ => {
                let len = c.len_utf8();
                let start_byte = self.current_byte.saturating_sub(len);
                let end_byte = self.current_byte;
                let col_start = self.col.saturating_sub(1);
                let span = Span::new(start_byte, end_byte, self.line, col_start, self.col);
                self.errors
                    .push(ScanError::UnexpectedCharacter { ch: c, span });
            }
        }
    }

    fn scan_string(&mut self) {
        while self.peek() != '"' && !self.is_at_end() {
            if self.peek() == '\n' {
                self.line += 1;
                self.col = 0;
            }
            self.advance();
        }

        if self.is_at_end() {
            let start_byte = self.start_byte;
            let end_byte = self.current_byte;
            let span = Span::new(start_byte, end_byte, self.line, self.start_col, self.col);
            self.errors.push(ScanError::UnterminatedString { span });
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

        let text: &str = &self.source[self.start_byte..self.current_byte];
        let token_type = Self::keywords()
            .get(text)
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
