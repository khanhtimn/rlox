use crate::token::Token;
use ordered_float::OrderedFloat;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum LiteralKind {
    Number(OrderedFloat<f64>),
    String(String),
    Boolean(bool),
    Nil,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
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
        kind: LiteralKind,
    },
    Unary {
        operator: Token,
        right: Box<Expr>,
    },
    Logical {
        left: Box<Expr>,
        operator: Token,
        right: Box<Expr>,
    },
    Variable {
        name: Token,
    },
    Assign {
        name: Token,
        value: Box<Expr>,
    },
    Call {
        callee: Box<Expr>,
        paren: Token,
        arguments: Box<[Expr]>,
    },
    Get {
        object: Box<Expr>,
        name: Token,
    },
    Set {
        object: Box<Expr>,
        name: Token,
        value: Box<Expr>,
    },
    This {
        keyword: Token,
    },
    Super {
        keyword: Token,
        method: Token,
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

    pub fn literal(kind: LiteralKind) -> Self {
        Expr::Literal { kind }
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
            Expr::Literal { kind } => match kind {
                LiteralKind::Number(n) => {
                    let mut s = n.to_string();
                    if s.ends_with(".0") {
                        s.truncate(s.len() - 2);
                    }
                    write!(f, "{}", s)
                }
                LiteralKind::String(s) => write!(f, "\"{}\"", s),
                LiteralKind::Boolean(b) => write!(f, "{}", b),
                LiteralKind::Nil => write!(f, "nil"),
            },
            Expr::Unary { operator, right } => {
                write!(f, "({} {})", operator.lexeme, right)
            }
            Expr::Variable { name } => {
                write!(f, "{}", name.lexeme)
            }
            Expr::Assign { name, value } => {
                write!(f, "({} = {})", name.lexeme, value)
            }
            Expr::Logical {
                left,
                operator,
                right,
            } => {
                write!(f, "({} {} {})", operator.lexeme, left, right)
            }
            Expr::Call {
                callee,
                paren,
                arguments,
            } => {
                write!(
                    f,
                    "({} {} {})",
                    callee,
                    paren.lexeme,
                    arguments
                        .iter()
                        .map(|a| a.to_string())
                        .collect::<Vec<String>>()
                        .join(", ")
                )
            }
            Expr::Get { object, name } => {
                write!(f, "({}.{})", object, name.lexeme)
            }
            Expr::Set {
                object,
                name,
                value,
            } => {
                write!(f, "({}.{} = {})", object, name.lexeme, value)
            }
            Expr::This { keyword } => write!(f, "{}", keyword.lexeme),
            Expr::Super { keyword, method } => {
                write!(f, "({}.{})", keyword.lexeme, method.lexeme)
            }
        }
    }
}
