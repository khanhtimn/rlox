use std::collections::HashMap;

use crate::{
    ast::{Expr, Stmt},
    interpreter::{Interpreter, RuntimeError},
    token::Token,
    value::{ClassType, FunctionType},
};

pub struct Resolver<'a> {
    interpreter: &'a mut Interpreter,
    scopes: Vec<HashMap<String, bool>>,
    slots: Vec<HashMap<String, usize>>,
    current_function: FunctionType,
    current_class: ClassType,
}

impl<'a> Resolver<'a> {
    pub fn new(interpreter: &'a mut Interpreter) -> Self {
        Self {
            interpreter,
            scopes: Vec::new(),
            slots: Vec::new(),
            current_function: FunctionType::None,
            current_class: ClassType::None,
        }
    }

    pub fn resolve_statements(&mut self, statements: &[Stmt]) -> Result<(), RuntimeError> {
        for statement in statements.iter() {
            self.resolve_statement(statement)?;
        }
        Ok(())
    }

    pub fn resolve_statement(&mut self, statement: &Stmt) -> Result<(), RuntimeError> {
        match statement {
            Stmt::Expr { expression } => self.resolve_expression(expression),
            Stmt::Var { name, initializer } => {
                self.declare(name.clone())?;
                if let Some(initializer) = initializer {
                    self.resolve_expression(initializer)?;
                }
                self.define(name.clone())?;
                Ok(())
            }
            Stmt::Block { statements } => {
                self.begin_scope();
                self.resolve_statements(statements)?;
                self.end_scope();
                Ok(())
            }
            Stmt::If {
                condition,
                then_branch,
                else_branch,
            } => {
                self.resolve_expression(condition)?;
                self.resolve_statement(then_branch)?;
                if let Some(else_branch) = else_branch {
                    self.resolve_statement(else_branch)?;
                }
                Ok(())
            }
            Stmt::While { condition, body } => {
                self.resolve_expression(condition)?;
                self.resolve_statement(body)?;
                Ok(())
            }
            Stmt::Return { keyword, value } => {
                if self.current_function == FunctionType::None {
                    return Err(RuntimeError::InvalidReturn {
                        span: Some(keyword.span),
                    });
                }
                if self.current_function == FunctionType::Initializer {
                    if value.is_some() {
                        return Err(RuntimeError::InitializerReturn {
                            span: Some(keyword.span),
                        });
                    }
                }
                if let Some(value) = value {
                    self.resolve_expression(value)?;
                }
                Ok(())
            }
            Stmt::Function { name, .. } => {
                self.declare(name.clone())?;
                self.define(name.clone())?;
                self.resolve_function(statement, FunctionType::Function)?;

                Ok(())
            }
            Stmt::Class { name, .. } => {
                self.declare(name.clone())?;
                self.define(name.clone())?;
                self.resolve_class(statement)?;

                Ok(())
            }
        }
    }

    pub fn resolve_expression(&mut self, expression: &Expr) -> Result<(), RuntimeError> {
        match expression {
            Expr::Binary { left, right, .. } => {
                self.resolve_expression(left)?;
                self.resolve_expression(right)?
            }
            Expr::Grouping { expression } => self.resolve_expression(expression)?,
            Expr::Literal { .. } => {}
            Expr::Unary { right, .. } => self.resolve_expression(right)?,
            Expr::Logical { left, right, .. } => {
                self.resolve_expression(left)?;
                self.resolve_expression(right)?
            }
            Expr::Variable { name, .. } => {
                if let Some(scope) = self.scopes.last() {
                    if let Some(&false) = scope.get(&name.lexeme) {
                        return Err(RuntimeError::VariableInitError {
                            name: name.lexeme.clone(),
                            span: Some(name.span),
                        });
                    }
                }
                self.resolve_local(expression, name);
            }
            Expr::Assign { name, value } => {
                self.resolve_expression(value)?;
                self.resolve_local(expression, name);
            }
            Expr::Call {
                callee, arguments, ..
            } => {
                self.resolve_expression(callee)?;
                for arg in arguments.iter() {
                    self.resolve_expression(arg)?;
                }
            }
            Expr::Get { object, .. } => self.resolve_expression(object)?,
            Expr::Set { object, value, .. } => {
                self.resolve_expression(value)?;
                self.resolve_expression(object)?;
            }
            Expr::This { keyword } => {
                if self.current_class == ClassType::None {
                    return Err(RuntimeError::ThisOutsideClass {
                        span: Some(keyword.span),
                    });
                }
                self.resolve_local(expression, keyword)
            }
            Expr::Super { keyword, method: _ } => {
                if self.current_class == ClassType::None {
                    return Err(RuntimeError::SuperOutsideClass {
                        span: Some(keyword.span),
                    });
                }
                if self.current_class != ClassType::Subclass {
                    return Err(RuntimeError::SuperNoSuperclass {
                        span: Some(keyword.span),
                    });
                }
                self.resolve_local(expression, keyword);
            }
        }
        Ok(())
    }

    fn resolve_function(
        &mut self,
        function: &Stmt,
        kind: FunctionType,
    ) -> Result<(), RuntimeError> {
        if let Stmt::Function {
            name: _,
            parameters,
            body,
        } = function
        {
            let enclosing_function = self.current_function;
            self.current_function = kind;
            self.begin_scope();
            for parameter in parameters {
                self.declare(parameter.clone())?;
                self.define(parameter.clone())?;
            }
            self.resolve_statements(body)?;
            self.end_scope();
            self.current_function = enclosing_function;
        }
        Ok(())
    }

    fn resolve_class(&mut self, class: &Stmt) -> Result<(), RuntimeError> {
        if let Stmt::Class {
            name,
            superclass,
            methods,
        } = class
        {
            let enclosing_class = self.current_class;
            self.current_class = ClassType::Class;

            if let Some(superclass) = superclass {
                if let Expr::Variable {
                    name: superclass_name,
                } = superclass
                {
                    if name.lexeme == superclass_name.lexeme {
                        return Err(RuntimeError::InternalResolverError {
                            message: "A class cannot inherit from itself.".to_string(),
                        });
                    }
                }
                self.current_class = ClassType::Subclass;
                self.resolve_expression(superclass)?;

                self.begin_scope();
                self.scopes
                    .last_mut()
                    .unwrap()
                    .insert("super".to_string(), true);
            }

            self.begin_scope();
            self.scopes
                .last_mut()
                .unwrap()
                .insert("this".to_string(), true);

            for method in methods.iter() {
                if let Stmt::Function { name: mname, .. } = method {
                    if mname.lexeme == "init" {
                        self.resolve_function(method, FunctionType::Initializer)?;
                        continue;
                    }
                }
                self.resolve_function(method, FunctionType::Method)?;
            }

            self.end_scope();

            if superclass.is_some() {
                self.end_scope();
            }

            self.current_class = enclosing_class;
        }
        Ok(())
    }

    fn resolve_local(&mut self, expr: &Expr, name: &Token) {
        for (i, scope) in self.scopes.iter().enumerate().rev() {
            if scope.contains_key(&name.lexeme) {
                let distance = self.scopes.len() - 1 - i;
                if let Some(slot) = self.slots[i].get(&name.lexeme).copied() {
                    let _ = self.interpreter.resolve_with_slot(expr, distance, slot);
                }
                return;
            }
        }
    }

    fn declare(&mut self, name: Token) -> Result<(), RuntimeError> {
        if let Some(scope) = self.scopes.last_mut() {
            if scope.contains_key(&name.lexeme) {
                return Err(RuntimeError::TypeError {
                    message: "Variable with this name already declared in this scope.".to_string(),
                    span: Some(name.span),
                });
            }
            let key = name.lexeme.clone();
            scope.insert(key.clone(), false);
            if let Some(slot_map) = self.slots.last_mut() {
                let next_index = slot_map.len();
                slot_map.insert(key, next_index);
                self.interpreter.register_decl_slot(&name, next_index);
            }
        }
        Ok(())
    }

    fn define(&mut self, name: Token) -> Result<(), RuntimeError> {
        if let Some(scope) = self.scopes.last_mut() {
            scope.insert(name.lexeme, true);
        }
        Ok(())
    }

    fn begin_scope(&mut self) {
        self.scopes.push(HashMap::new());
        self.slots.push(HashMap::new());
    }

    fn end_scope(&mut self) {
        self.scopes.pop();
        self.slots.pop();
    }
}
