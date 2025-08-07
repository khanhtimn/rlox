use clap::Parser;
use std::fs;
use std::io::{self, Write};
use std::process;
mod expression;
mod parser;
mod scanner;
mod token;

#[derive(Parser)]
#[command(name = "rlox")]
#[command(about = "A Rust implementation of the Lox programming language")]
struct Args {
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
            run(&source);
        }
        Err(err) => {
            eprintln!("Error reading file '{}': {}", path, err);
            process::exit(74);
        }
    }
}

fn run_prompt() {
    println!("Lox REPL - Enter expressions (Ctrl+C to exit)");

    loop {
        print!("> ");
        io::stdout().flush().unwrap();

        let mut line = String::new();
        match io::stdin().read_line(&mut line) {
            Ok(0) => break, // EOF
            Ok(_) => {
                let line = line.trim();
                if !line.is_empty() {
                    run(line);
                }
            }
            Err(err) => {
                eprintln!("Error reading input: {}", err);
                break;
            }
        }
    }
}

fn run(source: &str) {
    let tokens = scanner::tokenize(source);

    println!("=== TOKENS ===");
    for (i, token) in tokens.iter().enumerate() {
        println!("{:2}: {}", i + 1, token);
    }
    println!();

    println!("=== PARSING ===");
    match parser::parse(tokens) {
        Ok(expr) => println!("AST: {}", expr),
        Err(error) => eprintln!("Parse failed: {}", error),
    }
}
