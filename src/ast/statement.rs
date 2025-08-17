use crate::ast::Expr;
use crate::token::Token;

#[derive(Debug, Clone, PartialEq)]
pub enum Stmt {
    Expr {
        expression: Expr,
    },
    Var {
        name: Token,
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
    While {
        condition: Expr,
        body: Box<Stmt>,
    },
    Return {
        keyword: Token,
        value: Option<Expr>,
    },
    Function {
        name: Token,
        parameters: Vec<Token>,
        body: Vec<Stmt>,
    },
    Class {
        name: Token,
        superclass: Option<Expr>,
        methods: Vec<Stmt>,
    },
}

impl std::fmt::Display for Stmt {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
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
            Stmt::While { condition, body } => {
                write!(f, "while {} {{", condition)?;
                write!(f, "{}", body)?;
                write!(f, "}}")
            }
            Stmt::Return { keyword: _, value } => {
                if let Some(expr) = value {
                    write!(f, "return {};", expr)
                } else {
                    write!(f, "return;")
                }
            }
            Stmt::Function {
                name,
                parameters,
                body,
            } => {
                write!(
                    f,
                    "fn {} ({}) {{",
                    name,
                    parameters
                        .iter()
                        .map(|p| p.lexeme.clone())
                        .collect::<Vec<String>>()
                        .join(", ")
                )?;
                for statement in body {
                    write!(f, "{}", statement)?;
                }
                write!(f, "}}")
            }
            Stmt::Class {
                name,
                superclass,
                methods,
            } => {
                write!(f, "class {} {{", name)?;
                if let Some(superclass) = superclass {
                    write!(f, "superclass: {}", superclass)?;
                }
                for method in methods {
                    write!(f, "{}", method)?;
                }
                write!(f, "}}")
            }
        }
    }
}
