use clap::Parser as ClapParser;
use colored::Colorize;
use std::{
    error::Error,
    fs,
    io::{self, Write},
    process,
};

extern crate rizon_frontend;

use rizon_frontend::vm::Vm;

// --------
//   Cli
// --------

#[derive(ClapParser)]
#[command(version)]
#[command(about = "Interpreter for Rizon language")]
struct Cli {
    /// Path to the file to parse
    file: Option<String>,
}

fn main() -> Result<(), Box<dyn Error>> {
    let cli = Cli::parse();

    if let Some(f) = cli.file {
        run_file(f)?;
    } else {
        repl()?;
    }

    Ok(())
}

fn run_file(file_path: String,) -> Result<(), Box<dyn Error>> {
    let mut vm = Vm::new();
    vm.initialize();
    let code = fs::read_to_string(file_path)?;
    _ = vm.interpret(&code);

    Ok(())
}

fn repl() -> Result<(), Box<dyn Error>> {
    let stdin = io::stdin();
    let mut stdout = io::stdout();
    let mut input = String::new();

    println!("\n  {}", "Rizon language interpreter v0.0\n".yellow());

    let mut vm = Vm::new();
    vm.initialize();

    loop {
        input.clear();
        print!("> ");
        stdout.flush().unwrap();

        stdin.read_line(&mut input)?;
        
        let trimmed_input = input.trim();

        if trimmed_input == "quit" {
            process::exit(0);
        }

        if trimmed_input.is_empty() {
            continue;
        }

        // Execute interpreter
        _ = vm.interpret(trimmed_input);
    }
}
