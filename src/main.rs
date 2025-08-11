use clap::Parser;
use rustyline::{DefaultEditor, error::ReadlineError};
use std::fs;
use std::process;

use rlox::*;

#[derive(Parser)]
#[command(name = "rlox")]
#[command(about = "A Rust implementation of the Lox programming language")]
#[command(version = env!("CARGO_PKG_VERSION"))]
struct Args {
    #[arg(help = "Script file to execute. If not provided, starts an interactive REPL")]
    script: Option<String>,
}

fn main() {
    let args = Args::parse();

    match args.script {
        Some(script_path) => run_file(&script_path),
        None => run_prompt(),
    }
}

fn run_file(path: &str) {
    match fs::read_to_string(path) {
        Ok(source) => {
            let mut interpreter = interpreter::Interpreter::new();
            if let Err(err) = run_with(&mut interpreter, &source) {
                eprintln!("{}", rlox::diagnostics::render_error(err, &source, path));
            }
        }
        Err(err) => {
            eprintln!("Error reading file '{}': {}", path, err);
            process::exit(74);
        }
    }
}

fn run_prompt() {
    println!("RLox - A Rust implementation of the Lox programming language");
    println!("Press Ctrl+C or Ctrl+D to exit.");

    let mut rl = DefaultEditor::new().expect("failed to initialize line editor");
    let mut interpreter = interpreter::Interpreter::new();
    loop {
        match rl.readline("> ") {
            Ok(line) => {
                let trimmed = line.trim();
                if !trimmed.is_empty() {
                    rl.add_history_entry(line.as_str()).ok();
                    if let Err(err) = run_with(&mut interpreter, trimmed) {
                        eprintln!(
                            "{}",
                            rlox::diagnostics::render_error(err, trimmed, "<repl>")
                        );
                    }
                }
            }
            Err(ReadlineError::Interrupted) => {
                break;
            }
            Err(ReadlineError::Eof) => {
                break;
            }
            Err(err) => {
                eprintln!("Error reading input: {}", err);
                break;
            }
        }
    }
}
