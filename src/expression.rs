use crate::token::{LiteralKind, Token};

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
        value: LiteralKind,
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

    pub fn literal(value: LiteralKind) -> Self {
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
    use crate::token::{LiteralKind, Token, TokenKind};

    #[test]
    fn test_literal_expression() {
        let expr = Expr::literal(LiteralKind::Int);
        assert_eq!(expr.to_string(), "int");
    }

    #[test]
    fn test_unary_expression() {
        let token = Token::new(TokenKind::Minus, "-".to_string(), None);
        let expr = Expr::unary(token, Expr::literal(LiteralKind::Int));
        assert_eq!(expr.to_string(), "(- int)");
    }

    #[test]
    fn test_binary_expression() {
        let token = Token::new(TokenKind::Plus, "+".to_string(), None);
        let left = Expr::literal(LiteralKind::Int);
        let right = Expr::literal(LiteralKind::Decimal);
        let expr = Expr::binary(left, token, right);
        assert_eq!(expr.to_string(), "(+ int decimal)");
    }
}
