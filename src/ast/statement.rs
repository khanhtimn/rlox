use crate::ast::Expr;

#[derive(Debug, Clone, PartialEq)]
pub enum Stmt {
    Print(Expr),
    Expr(Expr),
    Var(String, Option<Expr>),
    Block(Vec<Stmt>),
}

impl std::fmt::Display for Stmt {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Stmt::Print(expr) => write!(f, "print {}", expr),
            Stmt::Expr(expr) => write!(f, "{}", expr),
            Stmt::Var(name, Some(expr)) => write!(f, "{} = {}", name, expr),
            Stmt::Var(name, None) => write!(f, "{} = nil", name),
            Stmt::Block(statements) => {
                write!(f, "{{")?;
                for statement in statements {
                    write!(f, "{}", statement)?;
                }
                write!(f, "}}")
            }
        }
    }
}
