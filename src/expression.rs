use crate::token::Token;

#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    Binary {
        left: Box<Expr>,
        operator: Token,
        right: Box<Expr>,
    },
    Grouping {
        expression: Box<Expr>,
    },
    Literal {
        value: String,
    },
    Unary {
        operator: Token,
        right: Box<Expr>,
    },
}

impl Expr {
    pub fn binary(left: Expr, operator: Token, right: Expr) -> Self {
        Expr::Binary {
            left: Box::new(left),
            operator,
            right: Box::new(right),
        }
    }

    pub fn unary(operator: Token, right: Expr) -> Self {
        Expr::Unary {
            operator,
            right: Box::new(right),
        }
    }

    pub fn grouping(expression: Expr) -> Self {
        Expr::Grouping {
            expression: Box::new(expression),
        }
    }

    pub fn literal(value: String) -> Self {
        Expr::Literal { value }
    }
}

impl std::fmt::Display for Expr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Expr::Binary {
                left,
                operator,
                right,
            } => {
                write!(f, "({} {} {})", operator.lexeme, left, right)
            }
            Expr::Grouping { expression } => {
                write!(f, "(group {})", expression)
            }
            Expr::Literal { value } => {
                write!(f, "{}", value)
            }
            Expr::Unary { operator, right } => {
                write!(f, "({} {})", operator.lexeme, right)
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::token::{Token, TokenKind};

    #[test]
    fn test_literal_expression() {
        let expr = Expr::literal("42".to_string());
        assert_eq!(expr.to_string(), "42");
    }

    #[test]
    fn test_unary_expression() {
        let token = Token::new(TokenKind::Minus, "-".to_string(), 1);
        let expr = Expr::unary(token, Expr::literal("42".to_string()));
        assert_eq!(expr.to_string(), "(- 42)");
    }

    #[test]
    fn test_binary_expression() {
        let token = Token::new(TokenKind::Plus, "+".to_string(), 1);
        let left = Expr::literal("1".to_string());
        let right = Expr::literal("2".to_string());
        let expr = Expr::binary(left, token, right);
        assert_eq!(expr.to_string(), "(+ 1 2)");
    }
}
