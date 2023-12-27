#[macro_use]
extern crate log;

use lost_compile::{interpret::Interpreter, resolve::Resolver, run};
use std::{
    env, fs,
    io::{self, Write},
};

fn main() {
    pretty_env_logger::init();
    debug!("Logging enabled");

    let args: Vec<String> = env::args().skip(1).collect();
    trace!("Arguments: {args:#?}");

    match args.len() {
        0 => run_repl(),
        1 => run_file(&args[0]),
        _ => {
            eprintln!("Multiple input files provided, they will be run in the provided order");
            for arg in &args {
                run_file(arg);
            }
        }
    }
}

fn run_file(file_path: &str) {
    let source = fs::read_to_string(file_path).expect("failed to read file");
    let mut resolver = Resolver::new();
    let mut interpreter = Interpreter::new();
    if let Err(errs) = run(&source, &mut resolver, &mut interpreter) {
        errs.iter().for_each(|e| eprintln!("{e}"));
    }
}

const VERSION: &str = env!("CARGO_PKG_VERSION");
const AUTHORS: &str = env!("CARGO_PKG_AUTHORS");

fn run_repl() {
    println!(
        "Lost REPL v{} on {} ({}), Copyright (c) {}",
        VERSION,
        env::consts::OS,
        env::consts::ARCH,
        AUTHORS
    );
    let (stdin, mut stdout) = (io::stdin(), io::stdout());
    let mut resolver = Resolver::new();
    let mut interpreter = Interpreter::new();
    loop {
        let mut line = String::default();
        print!(">>> ");
        stdout.flush().expect("failed to flush stdout");
        let n = stdin.read_line(&mut line).expect("failed to read line");
        // If zero bytes are read, then exit (usually triggered by Ctrl-D)
        if n == 0 {
            break;
        }
        if let Err(errs) = run(&line, &mut resolver, &mut interpreter) {
            errs.iter().for_each(|e| eprintln!("{e}"));
        }
    }
}
