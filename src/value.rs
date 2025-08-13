use crate::{ast::Stmt, environment::EnvRef};

#[derive(Debug, Clone)]
pub enum Value {
    Number(f64),
    String(String),
    Boolean(bool),
    Nil,
    Callable(CallableValue),
}

#[derive(Debug, Clone)]
pub enum CallableValue {
    Function(Function),
    // Class(Class),
    NativeFunction(NativeFunction),
}

#[derive(Debug, Clone)]
pub struct Function {
    pub name: String,
    pub parameters: Vec<String>,
    pub body: Vec<Stmt>,
    pub closure: EnvRef,
    pub arity: usize,
}

#[derive(Debug, Clone)]
pub struct NativeFunction {
    pub name: &'static str,
    pub arity: usize,
    pub function: fn(Vec<Value>) -> Value,
}

impl CallableValue {
    pub fn arity(&self) -> usize {
        match self {
            CallableValue::Function(func) => func.arity,
            CallableValue::NativeFunction(func) => func.arity,
        }
    }
}

impl PartialEq for Value {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Value::Nil, Value::Nil) => true,
            (Value::Boolean(a), Value::Boolean(b)) => a == b,
            (Value::Number(a), Value::Number(b)) => a == b,
            (Value::String(a), Value::String(b)) => a == b,
            (Value::Callable(_), Value::Callable(_)) => false,
            _ => false,
        }
    }
}

impl std::fmt::Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Nil => write!(f, "nil"),
            Value::Boolean(b) => write!(f, "{}", b),
            Value::Number(n) => {
                let s = n.to_string();
                if s.ends_with(".0") {
                    write!(f, "{}", &s[..s.len() - 2])
                } else {
                    write!(f, "{}", s)
                }
            }
            Value::String(s) => write!(f, "{}", s),
            Value::Callable(c) => write!(f, "{}", c),
        }
    }
}

impl std::fmt::Display for CallableValue {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            CallableValue::Function(func) => write!(f, "<fn {}>", func.name),
            CallableValue::NativeFunction(func) => write!(f, "<native fn {}>", func.name),
        }
    }
}
