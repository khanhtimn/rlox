use crate::ast::*;
use crate::environment::{EnvRef, Environment};
use crate::token::*;
use crate::value::Value;
use thiserror::Error;

#[derive(Debug, Error)]
pub enum RuntimeError {
    #[error("Runtime error: Type error: {message}")]
    TypeError { message: String, span: Option<Span> },

    #[error("Runtime error: Undefined variable: {name}")]
    UndefinedVariable { name: String, span: Option<Span> },

    #[error("Runtime error: Division by zero")]
    DivisionByZero { span: Option<Span> },

    #[error("Runtime error: Invalid operator: {op}")]
    InvalidOperator { op: String, span: Option<Span> },
}

impl RuntimeError {
    pub fn span(&self) -> Option<Span> {
        match self {
            RuntimeError::TypeError { span, .. } => *span,
            RuntimeError::UndefinedVariable { span, .. } => *span,
            RuntimeError::DivisionByZero { span } => *span,
            RuntimeError::InvalidOperator { span, .. } => *span,
        }
    }
}

pub struct Interpreter {
    environment: EnvRef,
}

impl Interpreter {
    pub fn new() -> Self {
        Self {
            environment: Environment::new(),
        }
    }

    pub fn interpret(&mut self, statements: Vec<Stmt>) -> Result<(), RuntimeError> {
        for statement in &statements {
            self.execute(statement)?;
        }
        Ok(())
    }

    fn execute(&mut self, statement: &Stmt) -> Result<(), RuntimeError> {
        match statement {
            Stmt::Print { expression: expr } => {
                let value = self.evaluate(&expr)?;
                println!("{}", value);
                Ok(())
            }
            Stmt::Expr { expression: expr } => {
                let value = self.evaluate(&expr)?;
                println!("{}", value);
                Ok(())
            }
            Stmt::Var { name, initializer } => {
                self.eval_var_statement(name, initializer.as_ref())?;
                Ok(())
            }
            Stmt::Block { statements } => {
                self.eval_block(statements)?;
                Ok(())
            }
            Stmt::If {
                condition,
                then_branch,
                else_branch,
            } => {
                let cond = self.evaluate(condition)?;
                if !self.is_falsy(&cond) {
                    self.execute(then_branch)?;
                } else if let Some(else_b) = else_branch {
                    self.execute(else_b)?;
                }
                Ok(())
            }
        }
    }

    pub fn evaluate(&mut self, expr: &Expr) -> Result<Value, RuntimeError> {
        match expr {
            Expr::Literal { kind } => self.eval_literal(kind),
            Expr::Unary { operator, right } => self.eval_unary(operator, right),
            Expr::Binary {
                left,
                operator,
                right,
            } => self.eval_binary(left, operator, right),
            Expr::Grouping { expression } => self.evaluate(expression),
            Expr::Variable { name } => self.eval_var_expr(name),
            Expr::Assign { name, value } => self.eval_assign_expression(name, value),
            Expr::Logical {
                left,
                operator,
                right,
            } => self.eval_logical(left, operator, right),
        }
    }

    fn eval_assign_expression(
        &mut self,
        name: &Token,
        value_expr: &Expr,
    ) -> Result<Value, RuntimeError> {
        let value = self.evaluate(value_expr)?;
        self.environment
            .borrow_mut()
            .assign(&name.lexeme, value.clone(), Some(name.span))?;
        Ok(value)
    }

    fn eval_block(&mut self, statements: &Vec<Stmt>) -> Result<(), RuntimeError> {
        let child = Environment::new_enclosed(self.environment.clone());
        self.execute_block(statements, child)
    }

    fn execute_block(
        &mut self,
        statements: &Vec<Stmt>,
        environment: EnvRef,
    ) -> Result<(), RuntimeError> {
        let previous = self.environment.clone();
        self.environment = environment;
        let result = statements.iter().try_for_each(|s| self.execute(s));
        self.environment = previous;
        result
    }

    fn eval_var_statement(
        &mut self,
        name: &str,
        initializer: Option<&Expr>,
    ) -> Result<(), RuntimeError> {
        let value = if let Some(expr) = initializer {
            self.evaluate(expr)?
        } else {
            Value::Nil
        };

        self.environment
            .borrow_mut()
            .define(name.to_string(), value);
        Ok(())
    }

    fn eval_var_expr(&mut self, name: &Token) -> Result<Value, RuntimeError> {
        self.environment.borrow().get(&name.lexeme, Some(name.span))
    }
    fn eval_literal(&self, kind: &LiteralKind) -> Result<Value, RuntimeError> {
        Ok(match kind {
            LiteralKind::Nil => Value::Nil,
            LiteralKind::Boolean(b) => Value::Boolean(*b),
            LiteralKind::Number(n) => Value::Number(*n),
            LiteralKind::String(s) => Value::String(s.clone()),
        })
    }

    fn eval_unary(&mut self, operator: &Token, right: &Expr) -> Result<Value, RuntimeError> {
        let right_val = self.evaluate(right)?;

        match operator.kind {
            TokenKind::Minus => match right_val {
                Value::Number(n) => Ok(Value::Number(-n)),
                _ => Err(RuntimeError::TypeError {
                    message: format!("- expects a number, got {:?}", right_val),
                    span: Some(operator.span),
                }),
            },
            TokenKind::Bang => Ok(Value::Boolean(self.is_falsy(&right_val))),
            _ => Err(RuntimeError::InvalidOperator {
                op: operator.lexeme.clone(),
                span: Some(operator.span),
            }),
        }
    }

    fn eval_binary(
        &mut self,
        left: &Expr,
        operator: &Token,
        right: &Expr,
    ) -> Result<Value, RuntimeError> {
        let left_val = self.evaluate(left)?;
        let right_val = self.evaluate(right)?;

        match operator.kind {
            TokenKind::Plus => match (left_val, right_val) {
                (Value::Number(a), Value::Number(b)) => Ok(Value::Number(a + b)),
                (Value::String(a), Value::String(b)) => Ok(Value::String(a + &b)),
                (Value::String(a), rv) => Ok(Value::String(a + &format!("{}", rv))),
                (lv, Value::String(b)) => Ok(Value::String(format!("{}", lv) + &b)),
                (lv, rv) => Err(RuntimeError::TypeError {
                    message: format!("+ expects numbers or strings, got {:?} and {:?}", lv, rv),
                    span: Some(operator.span),
                }),
            },

            TokenKind::Minus => match (left_val, right_val) {
                (Value::Number(a), Value::Number(b)) => Ok(Value::Number(a - b)),
                (lv, rv) => Err(RuntimeError::TypeError {
                    message: format!("- expects two numbers, got {:?} and {:?}", lv, rv),
                    span: Some(operator.span),
                }),
            },

            TokenKind::Star => match (left_val, right_val) {
                (Value::Number(a), Value::Number(b)) => Ok(Value::Number(a * b)),
                (lv, rv) => Err(RuntimeError::TypeError {
                    message: format!("* expects two numbers, got {:?} and {:?}", lv, rv),
                    span: Some(operator.span),
                }),
            },

            TokenKind::Slash => match (left_val, right_val) {
                (Value::Number(_a), Value::Number(b)) if b == 0.0 => {
                    Err(RuntimeError::DivisionByZero {
                        span: Some(operator.span),
                    })
                }
                (Value::Number(a), Value::Number(b)) => Ok(Value::Number(a / b)),
                (lv, rv) => Err(RuntimeError::TypeError {
                    message: format!("/ expects two numbers, got {:?} and {:?}", lv, rv),
                    span: Some(operator.span),
                }),
            },

            TokenKind::Greater => {
                self.compare_numbers(left_val, right_val, operator.span, |a, b| a > b)
            }
            TokenKind::GreaterEqual => {
                self.compare_numbers(left_val, right_val, operator.span, |a, b| a >= b)
            }
            TokenKind::Less => {
                self.compare_numbers(left_val, right_val, operator.span, |a, b| a < b)
            }
            TokenKind::LessEqual => {
                self.compare_numbers(left_val, right_val, operator.span, |a, b| a <= b)
            }

            TokenKind::EqualEqual => Ok(Value::Boolean(self.is_equal(&left_val, &right_val))),
            TokenKind::BangEqual => Ok(Value::Boolean(!self.is_equal(&left_val, &right_val))),

            _ => Err(RuntimeError::InvalidOperator {
                op: operator.lexeme.clone(),
                span: Some(operator.span),
            }),
        }
    }

    fn eval_logical(
        &mut self,
        left: &Expr,
        operator: &Token,
        right: &Expr,
    ) -> Result<Value, RuntimeError> {
        let left_val = self.evaluate(left)?;

        match operator.kind {
            TokenKind::Or => {
                if !self.is_falsy(&left_val) {
                    Ok(left_val)
                } else {
                    self.evaluate(right)
                }
            }
            TokenKind::And => {
                if self.is_falsy(&left_val) {
                    Ok(left_val)
                } else {
                    self.evaluate(right)
                }
            }
            _ => Err(RuntimeError::InvalidOperator {
                op: operator.lexeme.clone(),
                span: Some(operator.span),
            }),
        }
    }
    fn compare_numbers<F>(
        &self,
        left: Value,
        right: Value,
        span: crate::token::Span,
        cmp: F,
    ) -> Result<Value, RuntimeError>
    where
        F: FnOnce(f64, f64) -> bool,
    {
        match (left, right) {
            (Value::Number(a), Value::Number(b)) => Ok(Value::Boolean(cmp(a, b))),
            (lv, rv) => Err(RuntimeError::TypeError {
                message: format!("Comparison expects two numbers, got {:?} and {:?}", lv, rv),
                span: Some(span),
            }),
        }
    }

    fn is_equal(&self, left: &Value, right: &Value) -> bool {
        match (left, right) {
            (Value::Nil, Value::Nil) => true,
            (Value::Nil, _) | (_, Value::Nil) => false,
            (Value::Boolean(a), Value::Boolean(b)) => a == b,
            (Value::Number(a), Value::Number(b)) => a == b,
            (Value::String(a), Value::String(b)) => a == b,
            _ => false,
        }
    }

    fn is_falsy(&self, value: &Value) -> bool {
        match value {
            Value::Nil => true,
            Value::Boolean(b) => !b,
            _ => false,
        }
    }
}
