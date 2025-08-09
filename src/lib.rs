pub mod ast;
pub mod diagnostics;
pub mod environment;
pub mod error;
pub mod interpreter;
pub mod parser;
pub mod scanner;
pub mod token;
pub mod value;

use crate::error::Result;

pub fn run(source: &str) -> Result<()> {
    let mut interp = interpreter::Interpreter::new();
    run_with(&mut interp, source)
}

pub fn run_with(interpreter: &mut interpreter::Interpreter, source: &str) -> Result<()> {
    let tokens = scanner::tokenize(source)?;
    let statements = parser::parse(tokens)?;
    interpreter.interpret(statements)?;
    Ok(())
}
