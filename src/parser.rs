use crate::ast::*;
use crate::token::*;
use thiserror::Error;

#[derive(Debug)]
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
                message,
                expected,
                token,
                ..
            } => match expected {
                Some(exp) => write!(f, "{}\nExpected: {}, found: {}", message, exp, token),
                None => write!(f, "{}\nFound: {}", message, token),
            },
        }
    }
}

impl std::error::Error for ParseError {}

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
        if self.match_token(TokenKind::Fun) {
            return self.function_declaration("function".to_string());
        }
        if self.match_token(TokenKind::Class) {
            return self.class_declaration();
        }
        self.statement()
    }

    fn class_declaration(&mut self) -> Result<Stmt, ParseError> {
        let name = self
            .consume(TokenKind::Identifier, "Expect class name.")?
            .clone();
        let superclass = if self.match_token(TokenKind::Less) {
            self.consume(TokenKind::Identifier, "Expect superclass name.")?;
            Some(Expr::Variable {
                name: self.previous_token().clone(),
            })
        } else {
            None
        };
        self.consume(TokenKind::LeftBrace, "Expect '{' before class body.")?;
        let mut methods = Vec::new();
        while !self.is_at_end() && !self.check(TokenKind::RightBrace) {
            methods.push(self.function_declaration("method".to_string())?);
        }
        self.consume(TokenKind::RightBrace, "Expect '}' after class body.")?;
        Ok(Stmt::Class {
            name,
            superclass,
            methods,
        })
    }

    fn function_declaration(&mut self, kind: String) -> Result<Stmt, ParseError> {
        let name = self
            .consume(TokenKind::Identifier, &format!("Expect {} name.", kind))?
            .clone();
        self.consume(
            TokenKind::LeftParen,
            &format!("Expect '(' after {} name.", kind),
        )?;
        let mut parameters = Vec::new();
        if !self.check(TokenKind::RightParen) {
            loop {
                if parameters.len() >= 255 {
                    self.error(&self.peek().clone(), "Can't have more than 255 parameters.");
                }
                let param = self.consume(TokenKind::Identifier, "Expect parameter name.")?;
                parameters.push(param.clone());
                if !self.match_token(TokenKind::Comma) {
                    break;
                }
            }
        }
        self.consume(
            TokenKind::RightParen,
            &format!("Expect ')' after {} parameters.", kind),
        )?;

        self.consume(
            TokenKind::LeftBrace,
            &format!("Expect '{{' before {} body.", kind),
        )?;
        let body = self.block()?;
        Ok(Stmt::Function {
            name,
            parameters,
            body,
        })
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
        Ok(Stmt::Var { name, initializer })
    }

    fn statement(&mut self) -> Result<Stmt, ParseError> {
        if self.match_token(TokenKind::LeftBrace) {
            let statements = self.block()?;
            Ok(Stmt::Block { statements })
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
            Ok(Stmt::If {
                condition,
                then_branch: Box::new(then_branch),
                else_branch,
            })
        } else if self.match_token(TokenKind::While) {
            self.consume(TokenKind::LeftParen, "Expect '(' after 'while'.")?;
            let condition = self.expression()?;
            self.consume(TokenKind::RightParen, "Expect ')' after condition.")?;
            let body = self.statement()?;
            Ok(Stmt::While {
                condition,
                body: Box::new(body),
            })
        } else if self.match_token(TokenKind::Return) {
            let keyword = self.previous_token().clone();
            let value = if !self.check(TokenKind::Semicolon) {
                Some(self.expression()?)
            } else {
                None
            };
            self.consume(TokenKind::Semicolon, "Expect ';' after return value.")?;
            Ok(Stmt::Return { keyword, value })
        } else if self.match_token(TokenKind::For) {
            self.consume(TokenKind::LeftParen, "Expect '(' after 'for'.")?;
            let initializer = if self.match_token(TokenKind::Semicolon) {
                None
            } else if self.match_token(TokenKind::Var) {
                Some(self.var_declaration()?)
            } else {
                let expr = self.expression()?;
                self.consume(TokenKind::Semicolon, "Expect ';' after for initializer.")?;
                Some(Stmt::Expr { expression: expr })
            };
            let condition = if !self.match_token(TokenKind::Semicolon) {
                Some(self.expression()?)
            } else {
                None
            };
            self.consume(TokenKind::Semicolon, "Expect ';' after loop condition.")?;
            let increment = if !self.match_token(TokenKind::RightParen) {
                Some(self.expression()?)
            } else {
                None
            };
            self.consume(TokenKind::RightParen, "Expect ')' after for clauses.")?;

            let mut body = self.statement()?;

            if let Some(inc) = increment {
                body = Stmt::Block {
                    statements: vec![body, Stmt::Expr { expression: inc }],
                };
            }

            let while_condition = condition.unwrap_or(Expr::literal(LiteralKind::Boolean(true)));
            let while_stmt = Stmt::While {
                condition: while_condition,
                body: Box::new(body),
            };

            if let Some(init) = initializer {
                Ok(Stmt::Block {
                    statements: vec![init, while_stmt],
                })
            } else {
                Ok(while_stmt)
            }
        } else {
            let expr = self.expression()?;
            self.consume(TokenKind::Semicolon, "Expect ';' after expression.")?;
            Ok(Stmt::Expr { expression: expr })
        }
    }

    fn block(&mut self) -> Result<Vec<Stmt>, ParseError> {
        let mut statements = Vec::new();
        while !self.is_at_end() && !self.check(TokenKind::RightBrace) {
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
            } else if let Expr::Get { object, name } = expr {
                return Ok(Expr::Set {
                    object: Box::new(*object),
                    name: name.clone(),
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
        self.call()
    }

    fn call(&mut self) -> Result<Expr, ParseError> {
        let mut expr = self.primary()?;
        loop {
            if self.match_token(TokenKind::LeftParen) {
                let mut arguments = Vec::new();
                if !self.check(TokenKind::RightParen) {
                    loop {
                        if arguments.len() >= 255 {
                            self.error(&self.peek().clone(), "Can't have more than 255 arguments.");
                        }
                        arguments.push(self.expression()?);
                        if !self.match_token(TokenKind::Comma) {
                            break;
                        }
                    }
                }
                let paren = self.consume(TokenKind::RightParen, "Expect ')' after arguments.")?;
                expr = Expr::Call {
                    callee: Box::new(expr),
                    paren: paren.clone(),
                    arguments: arguments.into_boxed_slice(),
                };
            } else if self.match_token(TokenKind::Dot) {
                let name =
                    self.consume(TokenKind::Identifier, "Expect property name after '.'.")?;
                expr = Expr::Get {
                    object: Box::new(expr),
                    name: name.clone(),
                };
            } else if self.match_token(TokenKind::Super) {
                let keyword = self.previous_token().clone();
                self.consume(TokenKind::Dot, "Expect '.' after 'super'.")?;
                let method =
                    self.consume(TokenKind::Identifier, "Expect superclass method name.")?;
                expr = Expr::Super {
                    keyword,
                    method: method.clone(),
                };
            } else {
                break;
            }
        }
        Ok(expr)
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

        if self.match_token(TokenKind::This) {
            return Ok(Expr::This {
                keyword: self.previous_token().clone(),
            });
        }

        match &self.peek().kind {
            TokenKind::Number => {
                let token = self.advance();
                let n: f64 = token.lexeme.parse().unwrap();
                return Ok(Expr::literal(LiteralKind::Number(
                    ordered_float::OrderedFloat(n),
                )));
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
        let expected_str: &'static str = match kind {
            TokenKind::Identifier => "identifier",
            TokenKind::LeftParen => "'('",
            TokenKind::RightParen => "')'",
            TokenKind::LeftBrace => "'{'",
            TokenKind::RightBrace => "'}'",
            TokenKind::Semicolon => "';'",
            TokenKind::Var => "'var'",
            _ => "token",
        };

        let peek_token = self.peek().clone();
        let mut span = Some((peek_token.span.start, peek_token.span.end));
        if matches!(
            kind,
            TokenKind::RightParen | TokenKind::RightBrace | TokenKind::Semicolon
        ) {
            if self.current > 0 {
                let prev = self.previous_token().span;
                span = Some((prev.end, prev.end));
            }
        }

        self.had_error = true;
        Err(ParseError::UnexpectedToken {
            token: peek_token.lexeme,
            line: peek_token.span.line,
            message: message.to_string(),
            expected: Some(expected_str),
            span,
        })
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
