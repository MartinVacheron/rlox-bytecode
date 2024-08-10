use clap::Parser as ClapParser;
use colored::Colorize;
use std::{
    error::Error,
    fs,
    io::{self, Write},
    process,
};

extern crate rizon_frontend;

use rizon_frontend::vm::{Vm, VmFlags};

// --------
//   Cli
// --------

#[derive(ClapParser)]
#[command(version)]
#[command(about = "Interpreter for Rizon language")]
struct Cli {
    /// Path to the file to parse
    file: Option<String>,

    #[arg(long)]
    /// Disassemble each instruction
    dis_instr: bool,

    #[arg(short, long)]
    /// Disassemble compiled code
    dis_code: bool,
}

fn main() -> Result<(), Box<dyn Error>> {
    let cli = Cli::parse();

    let vm_flags = VmFlags {
        disassemble_compiled: cli.dis_code,
        disassemble_instructions: cli.dis_instr,
    };

    if let Some(f) = cli.file {
        run_file(f, vm_flags)?;
    } else {
        repl(vm_flags)?;
    }

    Ok(())
}

fn run_file(file_path: String, vm_flags: VmFlags) -> Result<(), Box<dyn Error>> {
    let mut vm = Vm::new(vm_flags);
    let code = fs::read_to_string(file_path)?;
    vm.interpret(&code);

    Ok(())
}

fn repl(vm_flags: VmFlags) -> Result<(), Box<dyn Error>> {
    let stdin = io::stdin();
    let mut stdout = io::stdout();
    let mut input = String::new();

    println!("\n  {}", "Rizon language interpreter v0.0\n".yellow());

    let mut vm = Vm::new(vm_flags);

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
        vm.interpret(trimmed_input);
    }
}
