use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

use crate::interpreter::{Interpreter, RuntimeError};
use crate::{ast::Stmt, environment::Environment, token::Token};

#[derive(Debug, Clone)]
pub enum Value {
    Number(f64),
    String(String),
    Boolean(bool),
    Nil,
    Callable(CallableValue),
    Instance(Rc<RefCell<Instance>>),
}

#[derive(Debug, Clone)]
pub enum CallableValue {
    Function(Function),
    Class(Class),
    NativeFunction(NativeFunction),
}

#[derive(Debug, Clone)]
pub struct Class {
    pub name: Token,
    pub superclass: Option<Box<Class>>,
    pub methods: HashMap<String, Function>,
}

#[derive(Copy, Clone, Eq, PartialEq)]
pub enum FunctionType {
    None,
    Function,
    Method,
    Initializer,
}

#[derive(Copy, Clone, Eq, PartialEq)]
pub enum ClassType {
    None,
    Class,
    Subclass,
}

impl Class {
    pub fn find_method(&self, name: &str) -> Option<Function> {
        if let Some(f) = self.methods.get(name) {
            return Some(f.clone());
        }
        if let Some(ref superclass) = self.superclass {
            return superclass.find_method(name);
        }
        None
    }
}

#[derive(Debug, Clone)]
pub struct Instance {
    pub class: Class,
    pub fields: HashMap<String, Value>,
}

#[derive(Debug, Clone)]
pub struct Function {
    pub name: String,
    pub parameters: Vec<Token>,
    pub body: Vec<Stmt>,
    pub closure: Rc<RefCell<Environment>>,
    pub arity: usize,
}

#[derive(Debug, Clone)]
pub struct NativeFunction {
    pub name: &'static str,
    pub arity: usize,
    pub function: fn(&mut Interpreter, &[Value]) -> Result<Value, RuntimeError>,
}

impl Value {
    pub fn type_name(&self) -> &'static str {
        match self {
            Value::Number(_) => "number",
            Value::String(_) => "string",
            Value::Boolean(_) => "boolean",
            Value::Nil => "nil",
            Value::Callable(c) => match c {
                CallableValue::Function(_) | CallableValue::NativeFunction(_) => "function",
                CallableValue::Class(_) => "class",
            },
            Value::Instance(_) => "instance",
        }
    }
}

impl CallableValue {
    pub fn arity(&self) -> usize {
        match self {
            CallableValue::Function(func) => func.arity,
            CallableValue::NativeFunction(func) => func.arity,
            CallableValue::Class(class) => class.find_method("init").map(|f| f.arity).unwrap_or(0),
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
            Value::Instance(instance) => {
                write!(f, "<{} instance>", instance.borrow().class.name.lexeme)
            }
        }
    }
}

impl std::fmt::Display for CallableValue {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            CallableValue::Function(func) => write!(f, "<fn {}>", func.name),
            CallableValue::NativeFunction(func) => write!(f, "<native fn {}>", func.name),
            CallableValue::Class(class) => write!(f, "{}", class.name.lexeme),
        }
    }
}

impl std::fmt::Display for Class {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "<class {}>", self.name.lexeme)
    }
}

impl std::fmt::Display for Instance {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "<{} instance>", self.class.name.lexeme)
    }
}
