use crate::interpreter::RuntimeError;
use crate::token::Span;
use crate::value::Value;
use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

#[derive(Debug, Clone, PartialEq)]
pub struct Environment {
    values: HashMap<String, Value>,
    slots: Vec<Value>,
    enclosing: Option<Rc<RefCell<Environment>>>,
}

impl Environment {
    pub fn new() -> Rc<RefCell<Environment>> {
        Rc::new(RefCell::new(Environment {
            values: HashMap::new(),
            slots: Vec::new(),
            enclosing: None,
        }))
    }

    pub fn new_enclosed(enclosing: Rc<RefCell<Environment>>) -> Rc<RefCell<Environment>> {
        Rc::new(RefCell::new(Environment {
            values: HashMap::new(),
            slots: Vec::new(),
            enclosing: Some(enclosing),
        }))
    }

    pub fn define(&mut self, name: String, value: Value) {
        self.values.insert(name, value);
    }

    pub fn get(&self, name: &str, span: Option<Span>) -> Result<Value, RuntimeError> {
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

    pub fn get_at(
        env: &Rc<RefCell<Environment>>,
        distance: usize,
        name: &str,
        span: Option<Span>,
    ) -> Result<Value, RuntimeError> {
        let target = Self::ancestor(env, distance)?;
        target
            .borrow()
            .values
            .get(name)
            .cloned()
            .ok_or_else(|| RuntimeError::UndefinedVariable {
                name: name.to_string(),
                span,
            })
    }

    fn ancestor(
        env: &Rc<RefCell<Environment>>,
        distance: usize,
    ) -> Result<Rc<RefCell<Environment>>, RuntimeError> {
        let mut current = env.clone();
        for _ in 0..distance {
            let next = match current.borrow().enclosing.as_ref() {
                Some(enc) => enc.clone(),
                None => {
                    return Err(RuntimeError::InternalResolverError {
                        message: "Invalid scope distance provided by resolver".to_string(),
                    });
                }
            };
            current = next;
        }
        Ok(current)
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

    pub fn assign_at(
        env: &Rc<RefCell<Environment>>,
        distance: usize,
        name: &str,
        value: Value,
        span: Option<Span>,
    ) -> Result<(), RuntimeError> {
        let target = Self::ancestor(env, distance)?;
        target.borrow_mut().assign(name, value, span)
    }

    pub fn ensure_slot(&mut self, index: usize) {
        if self.slots.len() <= index {
            self.slots.resize(index + 1, Value::Nil);
        }
    }

    pub fn set_slot(&mut self, index: usize, value: Value) {
        self.ensure_slot(index);
        self.slots[index] = value;
    }

    pub fn get_slot(&self, index: usize) -> Value {
        if index < self.slots.len() {
            self.slots[index].clone()
        } else {
            Value::Nil
        }
    }

    pub fn get_at_slot(
        env: &Rc<RefCell<Environment>>,
        distance: usize,
        index: usize,
    ) -> Result<Value, RuntimeError> {
        let target = Self::ancestor(env, distance)?;
        Ok(target.borrow().get_slot(index))
    }

    pub fn assign_at_slot(
        env: &Rc<RefCell<Environment>>,
        distance: usize,
        index: usize,
        value: Value,
    ) -> Result<(), RuntimeError> {
        let target = Self::ancestor(env, distance)?;
        target.borrow_mut().set_slot(index, value);
        Ok(())
    }
}
