use crate::ast::*;
use crate::token::*;
use thiserror::Error;

#[derive(Error, Debug)]
pub enum ParseError {
    UnexpectedToken {
        token: String,
        line: usize,
        message: String,
        expected: Option<&'static str>,
        span: Option<(usize, usize)>,
    },
}

impl ParseError {
    pub fn primary_span(&self) -> Option<(usize, usize)> {
        match self {
            ParseError::UnexpectedToken { span, .. } => *span,
        }
    }

    pub fn line(&self) -> usize {
        match self {
            ParseError::UnexpectedToken { line, .. } => *line,
        }
    }
}

impl std::fmt::Display for ParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ParseError::UnexpectedToken {
                message, expected, ..
            } => {
                if let Some(exp) = expected {
                    write!(f, "Syntax error: {}\nexpected: {}", message, exp)
                } else {
                    write!(f, "Syntax error: {}", message)
                }
            }
        }
    }
}

#[derive(Debug, Error)]
#[error("parse failed with {0} error(s)")]
pub struct ParseErrors(pub usize, pub Vec<ParseError>);

pub fn parse(tokens: Vec<Token>) -> Result<Vec<Stmt>, ParseErrors> {
    let mut parser = Parser::new(tokens);
    parser.parse()
}

struct Parser {
    tokens: Vec<Token>,
    current: usize,
    had_error: bool,
}

impl Parser {
    fn new(tokens: Vec<Token>) -> Self {
        Self {
            tokens,
            current: 0,
            had_error: false,
        }
    }

    fn parse(&mut self) -> Result<Vec<Stmt>, ParseErrors> {
        let mut statements = Vec::new();
        let mut errors: Vec<ParseError> = Vec::new();
        while !self.is_at_end() {
            match self.declaration() {
                Ok(stmt) => statements.push(stmt),
                Err(err) => {
                    errors.push(err);
                    self.synchronize();
                }
            }
        }
        if errors.is_empty() {
            Ok(statements)
        } else {
            Err(ParseErrors(errors.len(), errors))
        }
    }

    fn declaration(&mut self) -> Result<Stmt, ParseError> {
        if self.match_token(TokenKind::Var) {
            return self.var_declaration();
        }
        self.statement()
    }

    fn var_declaration(&mut self) -> Result<Stmt, ParseError> {
        let name = self
            .consume(TokenKind::Identifier, "Expect variable name.")?
            .clone();
        let initializer = if self.match_token(TokenKind::Equal) {
            Some(self.expression()?)
        } else {
            None
        };
        self.consume(
            TokenKind::Semicolon,
            "Expect ';' after variable declaration.",
        )?;
        Ok(Stmt::Var {
            name: name.lexeme,
            initializer,
        })
    }

    fn statement(&mut self) -> Result<Stmt, ParseError> {
        let stmt = if self.match_token(TokenKind::Print) {
            let value = self.expression()?;
            self.consume(TokenKind::Semicolon, "Expect ';' after value.")?;
            Stmt::Print { expression: value }
        } else if self.match_token(TokenKind::LeftBrace) {
            let statements = self.block()?;
            Stmt::Block { statements }
        } else if self.match_token(TokenKind::If) {
            self.consume(TokenKind::LeftParen, "Expect '(' after 'if'.")?;
            let condition = self.expression()?;
            self.consume(TokenKind::RightParen, "Expect ')' after condition.")?;
            let then_branch = self.statement()?;
            let else_branch = if self.match_token(TokenKind::Else) {
                Some(Box::new(self.statement()?))
            } else {
                None
            };
            Stmt::If {
                condition,
                then_branch: Box::new(then_branch),
                else_branch,
            }
        } else {
            let expr = self.expression()?;
            self.consume(TokenKind::Semicolon, "Expect ';' after expression.")?;
            Stmt::Expr { expression: expr }
        };
        Ok(stmt)
    }

    fn block(&mut self) -> Result<Vec<Stmt>, ParseError> {
        let mut statements = Vec::new();
        while !self.is_at_end() && self.peek().kind != TokenKind::RightBrace {
            statements.push(self.declaration()?);
        }
        self.consume(TokenKind::RightBrace, "Expect '}' after block.")?;
        Ok(statements)
    }

    fn expression(&mut self) -> Result<Expr, ParseError> {
        self.assignment()
    }

    fn assignment(&mut self) -> Result<Expr, ParseError> {
        let expr = self.logical_or()?;
        if self.match_token(TokenKind::Equal) {
            let equals = self.previous_token().clone();
            let value = self.assignment()?;
            if let Expr::Variable { name } = expr {
                return Ok(Expr::Assign {
                    name,
                    value: Box::new(value),
                });
            }
            self.error(&equals, "Invalid assignment target.");
        }
        Ok(expr)
    }

    fn logical_or(&mut self) -> Result<Expr, ParseError> {
        let mut expr = self.logical_and()?;
        while self.match_token(TokenKind::Or) {
            let operator = self.previous_token().clone();
            let right = self.logical_and()?;
            expr = Expr::Logical {
                left: Box::new(expr),
                operator,
                right: Box::new(right),
            };
        }
        Ok(expr)
    }

    fn logical_and(&mut self) -> Result<Expr, ParseError> {
        let mut expr = self.equality()?;
        while self.match_token(TokenKind::And) {
            let operator = self.previous_token().clone();
            let right = self.equality()?;
            expr = Expr::Logical {
                left: Box::new(expr),
                operator,
                right: Box::new(right),
            };
        }
        Ok(expr)
    }

    fn equality(&mut self) -> Result<Expr, ParseError> {
        if self.match_token(TokenKind::BangEqual) || self.match_token(TokenKind::EqualEqual) {
            let operator = self.previous_token().clone();
            let error = self.error(
                &operator,
                &format!(
                    "Binary operator '{}' missing left-hand operand.",
                    operator.lexeme
                ),
            );
            let _ = self.comparison()?;
            return Err(error);
        }

        let mut expression = self.comparison()?;
        while self.match_token(TokenKind::BangEqual) || self.match_token(TokenKind::EqualEqual) {
            let operator = self.previous_token().clone();
            let right = self.comparison()?;
            expression = Expr::binary(expression, operator, right);
        }
        Ok(expression)
    }

    fn comparison(&mut self) -> Result<Expr, ParseError> {
        if self.match_token(TokenKind::Greater)
            || self.match_token(TokenKind::GreaterEqual)
            || self.match_token(TokenKind::Less)
            || self.match_token(TokenKind::LessEqual)
        {
            let operator = self.previous_token().clone();
            let error = self.error(
                &operator,
                &format!(
                    "Binary operator '{}' missing left-hand operand.",
                    operator.lexeme
                ),
            );
            let _ = self.term()?;
            return Err(error);
        }

        let mut expression = self.term()?;
        while self.match_token(TokenKind::Greater)
            || self.match_token(TokenKind::GreaterEqual)
            || self.match_token(TokenKind::Less)
            || self.match_token(TokenKind::LessEqual)
        {
            let operator = self.previous_token().clone();
            let right = self.term()?;
            expression = Expr::binary(expression, operator, right);
        }
        Ok(expression)
    }

    fn term(&mut self) -> Result<Expr, ParseError> {
        if self.match_token(TokenKind::Plus) {
            let operator = self.previous_token().clone();
            let error = self.error(
                &operator,
                &format!(
                    "Binary operator '{}' missing left-hand operand.",
                    operator.lexeme
                ),
            );
            let _ = self.factor()?;
            return Err(error);
        }

        let mut expression = self.factor()?;
        while self.match_token(TokenKind::Minus) || self.match_token(TokenKind::Plus) {
            let operator = self.previous_token().clone();
            let right = self.factor()?;
            expression = Expr::binary(expression, operator, right);
        }
        Ok(expression)
    }

    fn factor(&mut self) -> Result<Expr, ParseError> {
        if self.match_token(TokenKind::Slash) || self.match_token(TokenKind::Star) {
            let operator = self.previous_token().clone();
            let error = self.error(
                &operator,
                &format!(
                    "Binary operator '{}' missing left-hand operand.",
                    operator.lexeme
                ),
            );
            let _ = self.unary()?;
            return Err(error);
        }

        let mut expression = self.unary()?;
        while self.match_token(TokenKind::Slash) || self.match_token(TokenKind::Star) {
            let operator = self.previous_token().clone();
            let right = self.unary()?;
            expression = Expr::binary(expression, operator, right);
        }
        Ok(expression)
    }

    fn unary(&mut self) -> Result<Expr, ParseError> {
        if self.match_token(TokenKind::Bang) || self.match_token(TokenKind::Minus) {
            let operator = self.previous_token().clone();
            let right = self.unary()?;
            return Ok(Expr::unary(operator, right));
        }
        self.primary()
    }

    fn primary(&mut self) -> Result<Expr, ParseError> {
        if self.match_token(TokenKind::False) {
            return Ok(Expr::literal(LiteralKind::Boolean(false)));
        }
        if self.match_token(TokenKind::True) {
            return Ok(Expr::literal(LiteralKind::Boolean(true)));
        }
        if self.match_token(TokenKind::Nil) {
            return Ok(Expr::literal(LiteralKind::Nil));
        }

        match &self.peek().kind {
            TokenKind::Number => {
                let token = self.advance();
                let n: f64 = token.lexeme.parse().unwrap();
                return Ok(Expr::literal(LiteralKind::Number(n)));
            }
            TokenKind::String => {
                let token = self.advance();
                let raw = token.lexeme.clone();
                let s = raw.trim_matches('"').to_string();
                return Ok(Expr::literal(LiteralKind::String(s)));
            }
            _ => {}
        }

        if self.match_token(TokenKind::LeftParen) {
            let expr = self.expression()?;
            self.consume(TokenKind::RightParen, "Expect ')' after expression.")?;
            return Ok(Expr::grouping(expr));
        }

        if self.match_token(TokenKind::Identifier) {
            let name = self.previous_token().clone();
            return Ok(Expr::Variable { name });
        }

        let token = self.peek().clone();
        Err(self.error(&token, "Expect expression."))
    }

    fn advance(&mut self) -> &Token {
        if !self.is_at_end() {
            self.current += 1;
        }
        self.previous_token()
    }

    fn is_at_end(&self) -> bool {
        self.peek().kind == TokenKind::Eof
    }

    fn match_token(&mut self, kind: TokenKind) -> bool {
        if self.check(kind) {
            self.advance();
            return true;
        }
        false
    }

    fn check(&self, kind: TokenKind) -> bool {
        if self.is_at_end() {
            return false;
        }
        self.peek().kind == kind
    }

    fn consume(&mut self, kind: TokenKind, message: &str) -> Result<&Token, ParseError> {
        if self.check(kind) {
            return Ok(self.advance());
        }
        let token = self.peek().clone();
        let mut err = self.error(&token, message);
        let ParseError::UnexpectedToken { expected, .. } = &mut err;
        *expected = Some(match kind {
            TokenKind::Identifier => "identifier",
            TokenKind::LeftParen => "'('",
            TokenKind::RightParen => "')'",
            TokenKind::LeftBrace => "'{'",
            TokenKind::RightBrace => "'}'",
            TokenKind::Semicolon => "';'",
            TokenKind::Var => "'var'",
            TokenKind::Print => "'print'",
            _ => "token",
        });
        Err(err)
    }

    fn error(&mut self, token: &Token, message: &str) -> ParseError {
        self.had_error = true;
        ParseError::UnexpectedToken {
            token: token.lexeme.clone(),
            line: token.span.line,
            message: message.to_string(),
            expected: None,
            span: Some((token.span.start, token.span.end)),
        }
    }

    fn synchronize(&mut self) {
        self.advance();

        while !self.is_at_end() {
            if self.previous_token().kind == TokenKind::Semicolon {
                return;
            }

            match self.peek().kind {
                TokenKind::Class
                | TokenKind::Fun
                | TokenKind::Var
                | TokenKind::For
                | TokenKind::If
                | TokenKind::While
                | TokenKind::Print
                | TokenKind::Return => return,
                _ => {}
            }

            self.advance();
        }
    }

    fn peek(&self) -> &Token {
        &self.tokens[self.current]
    }

    fn previous_token(&self) -> &Token {
        &self.tokens[self.current - 1]
    }
}
