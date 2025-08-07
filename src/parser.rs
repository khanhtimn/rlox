use crate::expression::*;
use crate::token::*;
use thiserror::Error;

#[derive(Error, Debug)]
pub enum ParseError {
    #[error("Unexpected token '{token}' at line {line}: {message}")]
    UnexpectedToken {
        token: String,
        line: usize,
        message: String,
    },
    #[error("Unexpected end of file")]
    UnexpectedEof,
}

pub fn parse(tokens: Vec<Token>) -> Result<Expr, ParseError> {
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

    fn parse(&mut self) -> Result<Expr, ParseError> {
        match self.expression() {
            Ok(expr) => Ok(expr),
            Err(error) => {
                self.synchronize();
                Err(error)
            }
        }
    }

    fn expression(&mut self) -> Result<Expr, ParseError> {
        self.equality()
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
            return Ok(Expr::literal(LiteralKind::String));
        }

        if self.match_token(TokenKind::True) {
            return Ok(Expr::literal(LiteralKind::String));
        }

        if self.match_token(TokenKind::Nil) {
            return Ok(Expr::literal(LiteralKind::String));
        }

        match &self.peek().kind {
            TokenKind::Literal { kind } => {
                let literal_kind = *kind;
                self.advance();
                return Ok(Expr::literal(literal_kind));
            }
            _ => {}
        }

        if self.match_token(TokenKind::LeftParen) {
            let expr = self.expression()?;
            self.consume(TokenKind::RightParen, "Expect ')' after expression.")?;
            return Ok(Expr::grouping(expr));
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
        Err(self.error(&token, message))
    }

    fn error(&mut self, token: &Token, message: &str) -> ParseError {
        self.had_error = true;
        ParseError::UnexpectedToken {
            token: token.lexeme.clone(),
            line: token.line,
            message: message.to_string(),
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

#[cfg(test)]
mod tests {
    use super::*;

    fn create_token(kind: TokenKind, lexeme: &str, literal: Option<LiteralKind>) -> Token {
        Token::new(kind, lexeme.to_string(), literal, 1)
    }

    #[test]
    fn test_parse_number() {
        let tokens = vec![
            create_token(
                TokenKind::Literal {
                    kind: LiteralKind::Int,
                },
                "42",
                Some(LiteralKind::Int),
            ),
            create_token(TokenKind::Eof, "", None),
        ];
        let expr = parse(tokens).unwrap();
        assert_eq!(expr.to_string(), "int");
    }

    #[test]
    fn test_parse_binary_expression() {
        let tokens = vec![
            create_token(
                TokenKind::Literal {
                    kind: LiteralKind::Int,
                },
                "1",
                Some(LiteralKind::Int),
            ),
            create_token(TokenKind::Plus, "+", None),
            create_token(
                TokenKind::Literal {
                    kind: LiteralKind::Int,
                },
                "2",
                Some(LiteralKind::Int),
            ),
            create_token(TokenKind::Eof, "", None),
        ];
        let expr = parse(tokens).unwrap();
        assert_eq!(expr.to_string(), "(+ int int)");
    }

    #[test]
    fn test_parse_unary_expression() {
        let tokens = vec![
            create_token(TokenKind::Minus, "-", None),
            create_token(
                TokenKind::Literal {
                    kind: LiteralKind::Int,
                },
                "42",
                Some(LiteralKind::Int),
            ),
            create_token(TokenKind::Eof, "", None),
        ];
        let expr = parse(tokens).unwrap();
        assert_eq!(expr.to_string(), "(- int)");
    }

    #[test]
    fn test_parse_grouping() {
        let tokens = vec![
            create_token(TokenKind::LeftParen, "(", None),
            create_token(
                TokenKind::Literal {
                    kind: LiteralKind::Int,
                },
                "42",
                Some(LiteralKind::Int),
            ),
            create_token(TokenKind::RightParen, ")", None),
            create_token(TokenKind::Eof, "", None),
        ];
        let expr = parse(tokens).unwrap();
        assert_eq!(expr.to_string(), "(group int)");
    }
}
