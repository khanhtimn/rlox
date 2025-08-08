use crate::expression::*;
use crate::token::*;
use thiserror::Error;

#[derive(Debug, Error)]
pub enum RuntimeError {
    #[error("Type error: {0}")]
    TypeError(String),

    #[error("Division by zero")]
    DivisionByZero,

    #[error("Invalid operator: {0}")]
    InvalidOperator(String),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    Number(f64),
    String(String),
    Boolean(bool),
    Nil,
}
pub struct Interpreter;

impl Interpreter {
    pub fn evaluate(&mut self, expr: &Expr) -> Result<Value, RuntimeError> {
        match expr {
            Expr::Literal { value } => self.eval_literal(value),
            Expr::Unary { operator, right } => self.eval_unary(operator, right),
            Expr::Binary {
                left,
                operator,
                right,
            } => self.eval_binary(left, operator, right),
            Expr::Grouping { expression } => self.evaluate(expression),
        }
    }

    fn eval_literal(&self, lexeme: &str) -> Result<Value, RuntimeError> {
        match lexeme {
            "true" => return Ok(Value::Boolean(true)),
            "false" => return Ok(Value::Boolean(false)),
            "nil" => return Ok(Value::Nil),
            _ => {}
        }

        if let Ok(int_val) = lexeme.parse::<i64>() {
            return Ok(Value::Number(int_val as f64));
        }
        if let Ok(float_val) = lexeme.parse::<f64>() {
            return Ok(Value::Number(float_val));
        }

        if lexeme.starts_with('"') && lexeme.ends_with('"') {
            let string_content = &lexeme[1..lexeme.len() - 1];
            return Ok(Value::String(string_content.to_string()));
        }

        Ok(Value::String(lexeme.to_string()))
    }

    fn eval_unary(&mut self, operator: &Token, right: &Expr) -> Result<Value, RuntimeError> {
        let right_val = self.evaluate(right)?;

        match operator.kind {
            TokenKind::Minus => match right_val {
                Value::Number(n) => Ok(Value::Number(-n)),
                _ => Err(RuntimeError::TypeError(
                    "Operand must be a number for unary minus".to_string(),
                )),
            },
            TokenKind::Bang => Ok(Value::Boolean(self.is_falsy(&right_val))),
            _ => Err(RuntimeError::InvalidOperator(format!(
                "Invalid unary operator: {}",
                operator.lexeme
            ))),
        }
    }

    fn is_falsy(&self, value: &Value) -> bool {
        match value {
            Value::Nil => true,
            Value::Boolean(b) => !b,
            _ => false,
        }
    }

    fn eval_binary(
        &mut self,
        _left: &Expr,
        _operator: &Token,
        _right: &Expr,
    ) -> Result<Value, RuntimeError> {
        todo!()
    }
}
