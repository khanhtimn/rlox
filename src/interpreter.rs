use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

use crate::ast::*;
use crate::environment::Environment;
use crate::token::*;
use crate::value::{CallableValue, Class, Function, Instance, Value};
use thiserror::Error;

#[derive(Debug, Error)]
pub enum RuntimeError {
    #[error("Type error: {message}")]
    TypeError { message: String, span: Option<Span> },

    #[error("Undefined variable: {name}")]
    UndefinedVariable { name: String, span: Option<Span> },

    #[error("Division by zero")]
    DivisionByZero { span: Option<Span> },

    #[error("Invalid operator: {op}")]
    InvalidOperator { op: String, span: Option<Span> },

    #[error("Not callable: {value_type}")]
    NotCallable {
        value_type: String,
        span: Option<Span>,
    },

    #[error("Arity mismatch: expected {expected}, got {got}")]
    ArityMismatch {
        expected: usize,
        got: usize,
        span: Option<Span>,
    },

    #[error("Operand type mismatch for '{op}': left {left_type}, right {right_type}")]
    BinaryTypeError {
        op: String,
        left_type: String,
        right_type: String,
        span: Option<Span>,
    },

    #[error("Operand type mismatch for unary '{op}': operand {operand_type}")]
    UnaryTypeError {
        op: String,
        operand_type: String,
        span: Option<Span>,
    },

    #[error("Invalid 'return' outside of function")]
    InvalidReturn { span: Option<Span> },

    #[error("Variable used in its own initializer: {name}")]
    VariableInitError { name: String, span: Option<Span> },

    #[error("Scope resolution failure: {message}")]
    ScopeResolutionError { message: String, span: Option<Span> },

    #[error("Undefined property: {name}")]
    UndefinedProperty { name: String, span: Option<Span> },

    #[error("Can't use 'this' outside of a class")]
    ThisOutsideClass { span: Option<Span> },

    #[error("Can't return a value from an initializer")]
    InitializerReturn { span: Option<Span> },

    #[error("return")]
    Return(Value),

    #[error("Internal resolver error: {message}")]
    InternalResolverError { message: String },
}

impl RuntimeError {
    pub fn span(&self) -> Option<Span> {
        match self {
            RuntimeError::TypeError { span, .. } => *span,
            RuntimeError::UndefinedVariable { span, .. } => *span,
            RuntimeError::DivisionByZero { span } => *span,
            RuntimeError::InvalidOperator { span, .. } => *span,
            RuntimeError::NotCallable { span, .. } => *span,
            RuntimeError::ArityMismatch { span, .. } => *span,
            RuntimeError::BinaryTypeError { span, .. } => *span,
            RuntimeError::UnaryTypeError { span, .. } => *span,
            RuntimeError::InvalidReturn { span } => *span,
            RuntimeError::VariableInitError { span, .. } => *span,
            RuntimeError::ScopeResolutionError { span, .. } => *span,
            RuntimeError::UndefinedProperty { span, .. } => *span,
            RuntimeError::ThisOutsideClass { span } => *span,
            RuntimeError::InitializerReturn { span } => *span,
            RuntimeError::Return(..) => None,
            RuntimeError::InternalResolverError { .. } => None,
        }
    }
}

#[derive(Clone, Copy, Debug)]
pub struct ResolvedLocal {
    pub depth: usize,
    pub slot: usize,
}

pub struct Interpreter {
    pub environment: Rc<RefCell<Environment>>,
    locals: HashMap<Expr, ResolvedLocal>,
    decl_slots: HashMap<Token, usize>,
}

impl Interpreter {
    pub fn new() -> Self {
        let env = Environment::new();
        crate::natives::install(&env);
        Self {
            environment: env,
            locals: HashMap::new(),
            decl_slots: HashMap::new(),
        }
    }
}

impl Interpreter {
    pub fn interpret(&mut self, statements: &[Stmt]) -> Result<(), RuntimeError> {
        for statement in statements {
            self.execute(statement)?;
        }
        Ok(())
    }

    pub fn execute(&mut self, statement: &Stmt) -> Result<(), RuntimeError> {
        match statement {
            Stmt::Expr { expression: expr } => {
                self.evaluate(&expr)?;
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
                self.eval_if(condition, then_branch, else_branch.as_deref())?;
                Ok(())
            }
            Stmt::While { condition, body } => {
                self.eval_while(condition, body)?;
                Ok(())
            }
            Stmt::Return { keyword: _, value } => {
                let ret = if let Some(expr) = value {
                    self.evaluate(expr)?
                } else {
                    Value::Nil
                };
                return Err(RuntimeError::Return(ret));
            }
            Stmt::Function {
                name,
                parameters,
                body,
            } => {
                self.eval_function(name, parameters, body)?;
                Ok(())
            }
            Stmt::Class { name, methods } => {
                self.eval_class(name, methods)?;
                Ok(())
            }
        }
    }

    pub fn resolve(&mut self, expr: &Expr, depth: usize) -> Result<(), RuntimeError> {
        self.locals
            .insert(expr.clone(), ResolvedLocal { depth, slot: 0 });
        Ok(())
    }

    pub fn resolve_with_slot(
        &mut self,
        expr: &Expr,
        depth: usize,
        slot: usize,
    ) -> Result<(), RuntimeError> {
        self.locals
            .insert(expr.clone(), ResolvedLocal { depth, slot });
        Ok(())
    }

    pub fn register_decl_slot(&mut self, token: &Token, slot: usize) {
        self.decl_slots.insert(token.clone(), slot);
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
            Expr::Variable { name } => self.eval_var_expression(name, expr),
            Expr::Assign { name, value } => self.eval_assign_expression(expr, name, value),
            Expr::Logical {
                left,
                operator,
                right,
            } => self.eval_logical(left, operator, right),
            Expr::Call {
                callee,
                paren,
                arguments,
            } => self.eval_call(callee, paren, arguments),
            Expr::Get { object, name } => self.eval_get(object, name),
            Expr::Set {
                object,
                name,
                value,
            } => self.eval_set(object, name, value),
            Expr::This { keyword } => self.eval_this(expr, keyword),
        }
    }

    fn eval_this(&mut self, expr: &Expr, keyword: &Token) -> Result<Value, RuntimeError> {
        if let Some(loc) = self.locals.get(expr) {
            Environment::get_at(&self.environment, loc.depth, "this", Some(keyword.span))
        } else {
            self.environment.borrow().get("this", Some(keyword.span))
        }
    }

    fn eval_set(
        &mut self,
        object: &Expr,
        name: &Token,
        value: &Expr,
    ) -> Result<Value, RuntimeError> {
        let value_val = self.evaluate(value)?;
        let object_val = self.evaluate(object)?;
        match object_val {
            Value::Instance(instance) => {
                instance
                    .borrow_mut()
                    .fields
                    .insert(name.lexeme.clone(), value_val.clone());
                Ok(value_val)
            }
            _ => Err(RuntimeError::TypeError {
                message: "Only instances have properties.".to_string(),
                span: Some(name.span),
            }),
        }
    }

    fn eval_get(&mut self, object: &Expr, name: &Token) -> Result<Value, RuntimeError> {
        let object_val = self.evaluate(object)?;
        match object_val {
            Value::Instance(instance_ref) => {
                let instance = instance_ref.borrow();
                if let Some(v) = instance.fields.get(&name.lexeme) {
                    Ok(v.clone())
                } else if let Some(method) = instance.class.methods.get(&name.lexeme) {
                    let bound = self.bind_method(method, Value::Instance(instance_ref.clone()))?;
                    Ok(Value::Callable(CallableValue::Function(bound)))
                } else {
                    Err(RuntimeError::UndefinedProperty {
                        name: name.lexeme.clone(),
                        span: Some(name.span),
                    })
                }
            }
            _ => Err(RuntimeError::TypeError {
                message: "Only instances have properties.".to_string(),
                span: Some(name.span),
            }),
        }
    }

    fn bind_method(&self, method: &Function, this_value: Value) -> Result<Function, RuntimeError> {
        let bound_env = Environment::new_enclosed(method.closure.clone());
        bound_env
            .borrow_mut()
            .define("this".to_string(), this_value);
        Ok(Function {
            name: method.name.clone(),
            parameters: method.parameters.clone(),
            body: method.body.clone(),
            closure: bound_env,
            arity: method.arity,
        })
    }

    fn eval_class(&mut self, name: &Token, methods: &[Stmt]) -> Result<(), RuntimeError> {
        self.environment
            .borrow_mut()
            .define(name.lexeme.clone(), Value::Nil);
        let mut method_map = HashMap::new();
        for m in methods {
            if let Stmt::Function {
                name: mname,
                parameters,
                body,
            } = m
            {
                let func = Function {
                    name: mname.lexeme.clone(),
                    parameters: parameters.clone(),
                    body: body.clone(),
                    closure: self.environment.clone(),
                    arity: parameters.len(),
                };
                method_map.insert(mname.lexeme.clone(), func);
            }
        }
        let class = Value::Callable(CallableValue::Class(Class {
            name: name.clone(),
            methods: method_map,
        }));
        self.environment
            .borrow_mut()
            .assign(&name.lexeme, class, Some(name.span))?;
        Ok(())
    }

    fn eval_call(
        &mut self,
        callee: &Expr,
        paren: &Token,
        arguments: &[Expr],
    ) -> Result<Value, RuntimeError> {
        let callee_val = self.evaluate(callee)?;
        let arguments_val = arguments
            .iter()
            .map(|a| self.evaluate(a))
            .collect::<Result<Vec<Value>, RuntimeError>>()?;
        match callee_val {
            Value::Callable(func) => {
                if func.arity() != arguments_val.len() {
                    return Err(RuntimeError::ArityMismatch {
                        expected: func.arity(),
                        got: arguments_val.len(),
                        span: Some(paren.span),
                    });
                }
                match func {
                    CallableValue::NativeFunction(native) => {
                        (native.function)(self, &arguments_val)
                    }
                    CallableValue::Function(fun) => {
                        let call_env = Environment::new_enclosed(fun.closure.clone());
                        {
                            let mut env_mut = call_env.borrow_mut();
                            for (param, arg) in fun.parameters.iter().zip(arguments_val.into_iter())
                            {
                                if let Some(slot) = self.decl_slots.get(param) {
                                    env_mut.set_slot(*slot, arg);
                                } else {
                                    env_mut.define(param.lexeme.clone(), arg);
                                }
                            }
                        }
                        match self.execute_block(&fun.body, call_env) {
                            Ok(()) => Ok(Value::Nil),
                            Err(RuntimeError::Return(v)) => Ok(v),
                            Err(e) => Err(e),
                        }
                    }
                    CallableValue::Class(class) => {
                        let instance = Rc::new(RefCell::new(Instance {
                            class: class.clone(),
                            fields: HashMap::new(),
                        }));
                        if let Some(init) = class.methods.get("init") {
                            let bound =
                                self.bind_method(init, Value::Instance(instance.clone()))?;
                            let call_env = Environment::new_enclosed(bound.closure.clone());
                            {
                                let mut env_mut = call_env.borrow_mut();
                                for (param, arg) in
                                    bound.parameters.iter().zip(arguments_val.into_iter())
                                {
                                    if let Some(slot) = self.decl_slots.get(param) {
                                        env_mut.set_slot(*slot, arg);
                                    } else {
                                        env_mut.define(param.lexeme.clone(), arg);
                                    }
                                }
                            }
                            match self.execute_block(&bound.body, call_env) {
                                Ok(()) => {}
                                Err(RuntimeError::Return(_)) => {}
                                Err(e) => return Err(e),
                            }
                        }
                        Ok(Value::Instance(instance))
                    }
                }
            }
            _ => Err(RuntimeError::NotCallable {
                value_type: callee_val.type_name().to_string(),
                span: Some(paren.span),
            }),
        }
    }

    fn eval_function(
        &mut self,
        name: &Token,
        parameters: &[Token],
        body: &[Stmt],
    ) -> Result<(), RuntimeError> {
        let func = Function {
            name: name.lexeme.clone(),
            parameters: parameters.to_vec(),
            body: body.to_vec(),
            closure: self.environment.clone(),
            arity: parameters.len(),
        };
        let value = Value::Callable(CallableValue::Function(func));
        if let Some(slot) = self.decl_slots.get(name) {
            self.environment.borrow_mut().set_slot(*slot, value);
        } else {
            self.environment
                .borrow_mut()
                .define(name.lexeme.clone(), value);
        }
        Ok(())
    }

    fn eval_if(
        &mut self,
        condition: &Expr,
        then_branch: &Stmt,
        else_branch: Option<&Stmt>,
    ) -> Result<(), RuntimeError> {
        let cond = self.evaluate(condition)?;
        if !self.is_falsy(&cond) {
            self.execute(then_branch)?;
        } else if let Some(else_b) = else_branch {
            self.execute(else_b)?;
        }
        Ok(())
    }

    fn eval_while(&mut self, condition: &Expr, body: &Stmt) -> Result<(), RuntimeError> {
        loop {
            let cond_val = self.evaluate(condition)?;
            if self.is_falsy(&cond_val) {
                break;
            }
            self.execute(body)?;
        }
        Ok(())
    }

    fn eval_assign_expression(
        &mut self,
        expr: &Expr,
        name: &Token,
        value_expr: &Expr,
    ) -> Result<Value, RuntimeError> {
        let value = self.evaluate(value_expr)?;
        if let Some(loc) = self.locals.get(expr) {
            Environment::assign_at_slot(&self.environment, loc.depth, loc.slot, value.clone())?;
        } else {
            self.environment
                .borrow_mut()
                .assign(&name.lexeme, value.clone(), Some(name.span))?;
        }
        Ok(value)
    }

    fn eval_block(&mut self, statements: &[Stmt]) -> Result<(), RuntimeError> {
        let child = Environment::new_enclosed(self.environment.clone());
        self.execute_block(statements, child)
    }

    fn execute_block(
        &mut self,
        statements: &[Stmt],
        environment: Rc<RefCell<Environment>>,
    ) -> Result<(), RuntimeError> {
        let previous = self.environment.clone();
        self.environment = environment;
        let result = statements.iter().try_for_each(|s| self.execute(s));
        self.environment = previous;
        result
    }

    fn eval_var_statement(
        &mut self,
        name: &Token,
        initializer: Option<&Expr>,
    ) -> Result<(), RuntimeError> {
        let value = if let Some(expr) = initializer {
            self.evaluate(expr)?
        } else {
            Value::Nil
        };

        if let Some(slot) = self.decl_slots.get(name) {
            self.environment.borrow_mut().set_slot(*slot, value);
        } else {
            self.environment
                .borrow_mut()
                .define(name.lexeme.clone(), value);
        }
        Ok(())
    }

    fn eval_var_expression(&mut self, name: &Token, expr: &Expr) -> Result<Value, RuntimeError> {
        if let Some(loc) = self.locals.get(expr) {
            Environment::get_at_slot(&self.environment, loc.depth, loc.slot)
        } else {
            self.environment.borrow().get(&name.lexeme, Some(name.span))
        }
    }

    fn eval_literal(&self, kind: &LiteralKind) -> Result<Value, RuntimeError> {
        Ok(match kind {
            LiteralKind::Nil => Value::Nil,
            LiteralKind::Boolean(b) => Value::Boolean(*b),
            LiteralKind::Number(n) => Value::Number(n.into_inner()),
            LiteralKind::String(s) => Value::String(s.clone()),
        })
    }

    fn eval_unary(&mut self, operator: &Token, right: &Expr) -> Result<Value, RuntimeError> {
        let right_val = self.evaluate(right)?;

        match operator.kind {
            TokenKind::Minus => match right_val {
                Value::Number(n) => Ok(Value::Number(-n)),
                other => Err(RuntimeError::UnaryTypeError {
                    op: "-".to_string(),
                    operand_type: other.type_name().to_string(),
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
                (lv, rv) => Err(RuntimeError::BinaryTypeError {
                    op: "+".to_string(),
                    left_type: lv.type_name().to_string(),
                    right_type: rv.type_name().to_string(),
                    span: Some(operator.span),
                }),
            },

            TokenKind::Minus => match (left_val, right_val) {
                (Value::Number(a), Value::Number(b)) => Ok(Value::Number(a - b)),
                (lv, rv) => Err(RuntimeError::BinaryTypeError {
                    op: "-".to_string(),
                    left_type: lv.type_name().to_string(),
                    right_type: rv.type_name().to_string(),
                    span: Some(operator.span),
                }),
            },

            TokenKind::Star => match (left_val, right_val) {
                (Value::Number(a), Value::Number(b)) => Ok(Value::Number(a * b)),
                (lv, rv) => Err(RuntimeError::BinaryTypeError {
                    op: "*".to_string(),
                    left_type: lv.type_name().to_string(),
                    right_type: rv.type_name().to_string(),
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
                (lv, rv) => Err(RuntimeError::BinaryTypeError {
                    op: "/".to_string(),
                    left_type: lv.type_name().to_string(),
                    right_type: rv.type_name().to_string(),
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
                message: format!(
                    "Comparison expects two numbers, got {} and {}",
                    lv.type_name(),
                    rv.type_name()
                ),
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
