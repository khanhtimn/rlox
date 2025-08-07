use crate::token::*;
use std::collections::HashMap;

pub fn tokenize(source: &str) -> Vec<Token> {
    let mut scanner = Scanner::new(source);
    scanner.scan_tokens()
}

struct Scanner<'a> {
    source: &'a str,
    tokens: Vec<Token>,
    start: usize,
    current: usize,
    line: usize,
}

impl<'a> Scanner<'a> {
    fn keywords() -> HashMap<&'static str, TokenKind> {
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
    }

    fn new(source: &'a str) -> Self {
        Self {
            source,
            tokens: Vec::new(),
            start: 0,
            current: 0,
            line: 1,
        }
    }

    fn scan_tokens(&mut self) -> Vec<Token> {
        while !self.is_at_end() {
            self.start = self.current;
            self.scan_token();
        }

        self.tokens
            .push(Token::new(TokenKind::Eof, "".to_string(), None));
        self.tokens.clone()
    }

    fn is_at_end(&self) -> bool {
        self.current >= self.source.len()
    }

    fn advance(&mut self) -> char {
        if let Some(c) = self.source.chars().nth(self.current) {
            self.current += c.len_utf8();
            c
        } else {
            '\0'
        }
    }

    fn peek(&self) -> char {
        self.source.chars().nth(self.current).unwrap_or('\0')
    }

    fn peek_next(&self) -> char {
        if self.current + 1 >= self.source.len() {
            '\0'
        } else {
            self.source.chars().nth(self.current + 1).unwrap_or('\0')
        }
    }

    fn is_alpha(&self, c: char) -> bool {
        (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || c == '_'
    }

    fn is_alpha_numeric(&self, c: char) -> bool {
        self.is_alpha(c) || c.is_ascii_digit()
    }

    fn match_char(&mut self, expected: char) -> bool {
        if self.is_at_end() || self.peek() != expected {
            false
        } else {
            self.current += expected.len_utf8();
            true
        }
    }

    fn add_token(&mut self, kind: TokenKind) {
        self.add_token_with_literal(kind, None);
    }

    fn add_token_with_literal(&mut self, kind: TokenKind, literal: Option<LiteralKind>) {
        let text = &self.source[self.start..self.current];
        self.tokens
            .push(Token::new(kind, text.to_string(), literal));
    }

    fn scan_token(&mut self) {
        let c = self.advance();

        match c {
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
            '/' => {
                if self.match_char('/') {
                    while self.peek() != '\n' && !self.is_at_end() {
                        self.advance();
                    }
                } else {
                    self.add_token(TokenKind::Slash);
                }
            }
            ' ' | '\r' | '\t' => {}
            '\n' => {
                self.line += 1;
            }
            '"' => {
                self.scan_string();
            }
            c if c.is_ascii_digit() => {
                self.scan_number();
            }
            c if self.is_alpha(c) => {
                self.identifier();
            }
            _ => {
                eprintln!("Unexpected character '{}' at line {}", c, self.line);
            }
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
            eprintln!("Unterminated string at line {}", self.line);
            return;
        }

        self.advance();

        let _value = &self.source[self.start + 1..self.current - 1];
        self.add_token_with_literal(TokenKind::String, Some(LiteralKind::String));
    }

    fn scan_number(&mut self) {
        while self.peek().is_ascii_digit() {
            self.advance();
        }

        let mut is_decimal = false;
        if self.peek() == '.' && self.peek_next().is_ascii_digit() {
            is_decimal = true;
            self.advance();

            while self.peek().is_ascii_digit() {
                self.advance();
            }
        }

        let literal_kind = if is_decimal {
            LiteralKind::Decimal
        } else {
            LiteralKind::Int
        };

        self.add_token_with_literal(TokenKind::Number, Some(literal_kind));
    }

    fn identifier(&mut self) {
        while self.is_alpha_numeric(self.peek()) {
            self.advance();
        }

        let text = &self.source[self.start..self.current];
        let keywords = Self::keywords();
        let token_type = keywords.get(text).copied().unwrap_or(TokenKind::Identifier);
        self.add_token(token_type);
    }
}
