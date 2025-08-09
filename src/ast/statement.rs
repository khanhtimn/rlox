use crate::ast::Expr;

#[derive(Debug, Clone, PartialEq)]
pub enum Stmt {
    Print {
        expression: Expr,
    },
    Expr {
        expression: Expr,
    },
    Var {
        name: String,
        initializer: Option<Expr>,
    },
    Block {
        statements: Vec<Stmt>,
    },
    If {
        condition: Expr,
        then_branch: Box<Stmt>,
        else_branch: Option<Box<Stmt>>,
    },
}

impl std::fmt::Display for Stmt {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Stmt::Print { expression } => write!(f, "print {}", expression),
            Stmt::Expr { expression } => write!(f, "{}", expression),
            Stmt::Var {
                name,
                initializer: Some(expr),
            } => write!(f, "{} = {}", name, expr),
            Stmt::Var {
                name,
                initializer: None,
            } => write!(f, "{} = nil", name),
            Stmt::Block { statements } => {
                write!(f, "{{")?;
                for statement in statements {
                    write!(f, "{}", statement)?;
                }
                write!(f, "}}")
            }
            Stmt::If {
                condition,
                then_branch,
                else_branch,
            } => {
                write!(f, "if {} {{", condition)?;
                write!(f, "{}", then_branch)?;
                if let Some(else_branch) = else_branch {
                    write!(f, "}} else {{")?;
                    write!(f, "{}", else_branch)?;
                }
                write!(f, "}}")
            }
        }
    }
}
