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

    #[error("Can't use 'super' outside of a class")]
    SuperOutsideClass { span: Option<Span> },

    #[error("Can't use 'super' in a class with no superclass")]
    SuperNoSuperclass { span: Option<Span> },

    #[error("Can't return a value from an initializer")]
    InitializerReturn { span: Option<Span> },

    #[error("return")]
    Return(Value),

    #[error("Internal resolver error: {message}")]
    InternalResolverError { message: String },

    #[error("Superclass must be a class")]
    SuperclassMustBeClass { span: Option<Span> },
}

impl RuntimeError {
    pub fn span(&self) -> Option<Span> {
        match self {
            RuntimeError::TypeError { span, .. }
            | RuntimeError::UndefinedVariable { span, .. }
            | RuntimeError::DivisionByZero { span }
            | RuntimeError::InvalidOperator { span, .. }
            | RuntimeError::NotCallable { span, .. }
            | RuntimeError::ArityMismatch { span, .. }
            | RuntimeError::BinaryTypeError { span, .. }
            | RuntimeError::UnaryTypeError { span, .. }
            | RuntimeError::InvalidReturn { span }
            | RuntimeError::VariableInitError { span, .. }
            | RuntimeError::ScopeResolutionError { span, .. }
            | RuntimeError::UndefinedProperty { span, .. }
            | RuntimeError::ThisOutsideClass { span }
            | RuntimeError::InitializerReturn { span }
            | RuntimeError::SuperclassMustBeClass { span }
            | RuntimeError::SuperOutsideClass { span }
            | RuntimeError::SuperNoSuperclass { span } => *span,
            RuntimeError::InternalResolverError { .. } | RuntimeError::Return(_) => None,
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
                let value = if let Some(expr) = initializer {
                    self.evaluate(expr)?
                } else {
                    Value::Nil
                };
                self.define_or_set_slot(name, value);
                Ok(())
            }
            Stmt::Block { statements } => {
                let child = Environment::new_enclosed(self.environment.clone());
                self.execute_block(statements, child)
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
            Stmt::While { condition, body } => {
                loop {
                    let cond_val = self.evaluate(condition)?;
                    if self.is_falsy(&cond_val) {
                        break;
                    }
                    self.execute(body)?;
                }
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
                let func = Function {
                    name: name.lexeme.clone(),
                    parameters: parameters.to_vec(),
                    body: body.to_vec(),
                    closure: self.environment.clone(),
                    arity: parameters.len(),
                };
                let value = Value::Callable(CallableValue::Function(func));
                self.define_or_set_slot(name, value);
                Ok(())
            }
            Stmt::Class {
                name,
                superclass,
                methods,
            } => {
                self.define_or_set_slot(name, Value::Nil);

                let superclass = if let Some(superclass_expr) = superclass {
                    match self.evaluate(superclass_expr)? {
                        Value::Callable(CallableValue::Class(class)) => Some(Box::new(class)),
                        _ => return Err(RuntimeError::SuperclassMustBeClass { span: None }),
                    }
                } else {
                    None
                };

                let method_closure_env = if let Some(ref sup) = superclass {
                    let env = Environment::new_enclosed(self.environment.clone());
                    env.borrow_mut().define(
                        "super".to_string(),
                        Value::Callable(CallableValue::Class(*sup.clone())),
                    );
                    env
                } else {
                    self.environment.clone()
                };

                let mut method_map = HashMap::new();
                for method in methods {
                    if let Stmt::Function {
                        name: mname,
                        parameters,
                        body,
                    } = method
                    {
                        let func = Function {
                            name: mname.lexeme.clone(),
                            parameters: parameters.clone(),
                            body: body.clone(),
                            closure: method_closure_env.clone(),
                            arity: parameters.len(),
                        };
                        method_map.insert(mname.lexeme.clone(), func);
                    }
                }

                let class = Value::Callable(CallableValue::Class(Class {
                    name: name.clone(),
                    superclass,
                    methods: method_map,
                }));

                if let Some(slot) = self.decl_slots.get(name) {
                    self.environment.borrow_mut().set_slot(*slot, class);
                } else {
                    self.environment
                        .borrow_mut()
                        .assign(&name.lexeme, class, Some(name.span))?;
                }
                Ok(())
            }
        }
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
            Expr::Variable { name } => self.eval_variable(expr, name),
            Expr::Assign { name, value } => self.eval_assign(expr, name, value),
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
            Expr::Super { keyword, method } => self.eval_super(expr, keyword, method),
        }
    }

    fn eval_variable(&self, expr: &Expr, name: &Token) -> Result<Value, RuntimeError> {
        if let Some(loc) = self.locals.get(expr) {
            Environment::get_at_slot(&self.environment, loc.depth, loc.slot)
        } else {
            self.environment.borrow().get(&name.lexeme, Some(name.span))
        }
    }

    fn eval_super(
        &mut self,
        expr: &Expr,
        keyword: &Token,
        method: &Token,
    ) -> Result<Value, RuntimeError> {
        let loc =
            self.locals
                .get(expr)
                .copied()
                .ok_or_else(|| RuntimeError::InternalResolverError {
                    message: "super not resolved".to_string(),
                })?;

        let super_val =
            Environment::get_at(&self.environment, loc.depth, "super", Some(keyword.span))?;
        let Value::Callable(CallableValue::Class(superclass)) = super_val else {
            return Err(RuntimeError::InternalResolverError {
                message: "super binding is not a class".to_string(),
            });
        };

        let this_val =
            Environment::get_at(&self.environment, loc.depth - 1, "this", Some(keyword.span))?;

        if let Some(func) = superclass.find_method(&method.lexeme) {
            let bound = self.bind_method(&func, this_val)?;
            Ok(Value::Callable(CallableValue::Function(bound)))
        } else {
            Err(RuntimeError::UndefinedProperty {
                name: method.lexeme.clone(),
                span: Some(method.span),
            })
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
                } else if let Some(method) = instance.class.find_method(&name.lexeme) {
                    let bound = self.bind_method(&method, Value::Instance(instance_ref.clone()))?;
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

        let Value::Callable(func) = callee_val else {
            return Err(RuntimeError::NotCallable {
                value_type: callee_val.type_name().to_string(),
                span: Some(paren.span),
            });
        };

        if func.arity() != arguments_val.len() {
            return Err(RuntimeError::ArityMismatch {
                expected: func.arity(),
                got: arguments_val.len(),
                span: Some(paren.span),
            });
        }

        match func {
            CallableValue::NativeFunction(native) => (native.function)(self, &arguments_val),
            CallableValue::Function(fun) => self.call_function(&fun, arguments_val),
            CallableValue::Class(class) => self.call_class(&class, arguments_val),
        }
    }

    fn call_function(
        &mut self,
        fun: &Function,
        arguments: Vec<Value>,
    ) -> Result<Value, RuntimeError> {
        let call_env = Environment::new_enclosed(fun.closure.clone());
        self.setup_function_parameters(&call_env, &fun.parameters, arguments);

        match self.execute_block(&fun.body, call_env) {
            Ok(()) => Ok(Value::Nil),
            Err(RuntimeError::Return(v)) => Ok(v),
            Err(e) => Err(e),
        }
    }

    fn call_class(&mut self, class: &Class, arguments: Vec<Value>) -> Result<Value, RuntimeError> {
        let instance = Rc::new(RefCell::new(Instance {
            class: class.clone(),
            fields: HashMap::new(),
        }));

        if let Some(init) = class.find_method("init") {
            let bound = self.bind_method(&init, Value::Instance(instance.clone()))?;
            let call_env = Environment::new_enclosed(bound.closure.clone());
            self.setup_function_parameters(&call_env, &bound.parameters, arguments);

            match self.execute_block(&bound.body, call_env) {
                Ok(()) => {}
                Err(RuntimeError::Return(_)) => {}
                Err(e) => return Err(e),
            }
        }
        Ok(Value::Instance(instance))
    }

    fn setup_function_parameters(
        &self,
        env: &Rc<RefCell<Environment>>,
        parameters: &[Token],
        arguments: Vec<Value>,
    ) {
        let mut env_mut = env.borrow_mut();
        for (param, arg) in parameters.iter().zip(arguments.into_iter()) {
            if let Some(slot) = self.decl_slots.get(param) {
                env_mut.set_slot(*slot, arg);
            } else {
                env_mut.define(param.lexeme.clone(), arg);
            }
        }
    }

    fn eval_assign(
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
            TokenKind::Plus => self.eval_addition(left_val, right_val, operator.span),
            TokenKind::Minus | TokenKind::Star | TokenKind::Slash => {
                self.eval_arithmetic(left_val, right_val, operator)
            }
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

    fn eval_addition(&self, left: Value, right: Value, span: Span) -> Result<Value, RuntimeError> {
        match (left, right) {
            (Value::Number(a), Value::Number(b)) => Ok(Value::Number(a + b)),
            (Value::String(a), Value::String(b)) => Ok(Value::String(a + &b)),
            (Value::String(a), rv) => Ok(Value::String(a + &format!("{}", rv))),
            (lv, Value::String(b)) => Ok(Value::String(format!("{}", lv) + &b)),
            (lv, rv) => Err(RuntimeError::BinaryTypeError {
                op: "+".to_string(),
                left_type: lv.type_name().to_string(),
                right_type: rv.type_name().to_string(),
                span: Some(span),
            }),
        }
    }

    fn eval_arithmetic(
        &self,
        left: Value,
        right: Value,
        operator: &Token,
    ) -> Result<Value, RuntimeError> {
        match (left, right) {
            (Value::Number(a), Value::Number(b)) => match operator.kind {
                TokenKind::Minus => Ok(Value::Number(a - b)),
                TokenKind::Star => Ok(Value::Number(a * b)),
                TokenKind::Slash => {
                    if b == 0.0 {
                        Err(RuntimeError::DivisionByZero {
                            span: Some(operator.span),
                        })
                    } else {
                        Ok(Value::Number(a / b))
                    }
                }
                _ => unreachable!(),
            },
            (lv, rv) => Err(RuntimeError::BinaryTypeError {
                op: operator.lexeme.clone(),
                left_type: lv.type_name().to_string(),
                right_type: rv.type_name().to_string(),
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

    fn define_or_set_slot(&mut self, name: &Token, value: Value) {
        if let Some(slot) = self.decl_slots.get(name) {
            self.environment.borrow_mut().set_slot(*slot, value);
        } else {
            self.environment
                .borrow_mut()
                .define(name.lexeme.clone(), value);
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
