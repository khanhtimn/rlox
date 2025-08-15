use std::cell::RefCell;
use std::rc::Rc;

use crate::environment::Environment;
use crate::interpreter::{Interpreter, RuntimeError};
use crate::value::{CallableValue, NativeFunction, Value};

pub fn install(env: &Rc<RefCell<Environment>>) {
    add_native_fn(
        env,
        NativeFunction {
            name: "print",
            arity: 1,
            function: print,
        },
    );
    add_native_fn(
        env,
        NativeFunction {
            name: "read",
            arity: 0,
            function: read,
        },
    );
    add_native_fn(
        env,
        NativeFunction {
            name: "clock",
            arity: 0,
            function: clock,
        },
    );
    add_native_fn(
        env,
        NativeFunction {
            name: "type_of",
            arity: 1,
            function: type_of,
        },
    );
}

fn add_native_fn(env: &Rc<RefCell<Environment>>, nf: NativeFunction) {
    env.borrow_mut().define(
        nf.name.to_string(),
        Value::Callable(CallableValue::NativeFunction(nf)),
    );
}

fn print(_: &mut Interpreter, args: &[Value]) -> Result<Value, RuntimeError> {
    if !args.is_empty() {
        let mut first = true;
        for v in args {
            if !first {
                print!(" ");
            }
            first = false;
            print!("{}", v);
        }
    }
    println!();
    Ok(Value::Nil)
}

fn read(_: &mut Interpreter, _: &[Value]) -> Result<Value, RuntimeError> {
    let mut buffer = String::new();
    std::io::stdin()
        .read_line(&mut buffer)
        .map_err(|e| RuntimeError::TypeError {
            message: format!("Failed to read from stdin: {}", e),
            span: None,
        })?;
    let trimmed = buffer.trim_end_matches(&['\n', '\r'][..]);

    if let Ok(num) = trimmed.parse::<f64>() {
        Ok(Value::Number(num))
    } else {
        Ok(Value::String(trimmed.to_string()))
    }
}

fn clock(_: &mut Interpreter, _: &[Value]) -> Result<Value, RuntimeError> {
    let now = std::time::SystemTime::now();
    let duration = now.duration_since(std::time::UNIX_EPOCH).unwrap();
    Ok(Value::Number(duration.as_secs_f64()))
}

fn type_of(_: &mut Interpreter, args: &[Value]) -> Result<Value, RuntimeError> {
    if let Some(value) = args.get(0) {
        Ok(Value::String(value.type_name().to_string()))
    } else {
        Ok(Value::String("nil".to_string()))
    }
}
