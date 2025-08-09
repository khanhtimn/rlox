use crate::interpreter::RuntimeError;
use crate::value::Value;
use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

#[derive(Debug, Clone, PartialEq)]
pub struct Environment {
    values: HashMap<String, Value>,
    enclosing: Option<EnvRef>,
}

pub type EnvRef = Rc<RefCell<Environment>>;

impl Environment {
    pub fn new() -> EnvRef {
        Rc::new(RefCell::new(Environment {
            values: HashMap::new(),
            enclosing: None,
        }))
    }

    pub fn new_enclosed(enclosing: EnvRef) -> EnvRef {
        Rc::new(RefCell::new(Environment {
            values: HashMap::new(),
            enclosing: Some(enclosing),
        }))
    }

    pub fn define(&mut self, name: String, value: Value) {
        self.values.insert(name, value);
    }

    pub fn get(&self, name: &str, span: Option<crate::token::Span>) -> Result<Value, RuntimeError> {
        if let Some(val) = self.values.get(name) {
            return Ok(val.clone());
        }
        if let Some(enclosing) = &self.enclosing {
            return enclosing.borrow().get(name, span);
        }
        Err(RuntimeError::UndefinedVariable {
            name: name.to_string(),
            span,
        })
    }

    pub fn assign(
        &mut self,
        name: &str,
        value: Value,
        span: Option<crate::token::Span>,
    ) -> Result<(), RuntimeError> {
        if self.values.contains_key(name) {
            self.values.insert(name.to_string(), value);
            return Ok(());
        }
        if let Some(enclosing) = &self.enclosing {
            return enclosing.borrow_mut().assign(name, value, span);
        }
        Err(RuntimeError::UndefinedVariable {
            name: name.to_string(),
            span,
        })
    }
}
