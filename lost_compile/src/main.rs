#[macro_use]
extern crate log;

mod environment;
mod error;
mod interpret;

use environment::Env;
use interpret::Interpreter;
use lost_syntax::{error::Error, lex::Lexer, parse::Parser};
use std::{
    env, fs,
    io::{self, Write},
};

const VERSION: &str = env!("CARGO_PKG_VERSION");
const AUTHORS: &str = env!("CARGO_PKG_AUTHORS");

fn main() {
    pretty_env_logger::init();
    info!("Logging enabled");

    let args: Vec<String> = env::args().skip(1).collect();
    debug!("Arguments: {args:#?}");
    if args.len() > 1 {
        panic!("Too many arguments");
    }
    if args.is_empty() {
        run_repl();
    } else {
        run_file(&args[0]);
    }
}

fn run_repl() {
    println!(
        "Lost REPL v{} on {} ({}), Copyright (c) {}",
        VERSION,
        env::consts::OS,
        env::consts::ARCH,
        AUTHORS
    );
    let (stdin, mut stdout) = (io::stdin(), io::stdout());
    let mut code = String::default();
    let mut env = Env::default();
    loop {
        let mut line = String::default();
        print!(">>> ");
        stdout.flush().expect("Failed to flush stdout");
        let n = stdin.read_line(&mut line).expect("Failed to read line");
        // If zero bytes are read, then exit (usually triggered by Ctrl-D)
        if n == 0 {
            break;
        }
        match run(&line, Some(env.clone())) {
            Ok(new_env) => {
                code.push_str(&line);
                env = new_env;
            }
            Err(errors) => errors.iter().for_each(|e| eprintln!("{e}")),
        }
    }
}

fn run_file(file_path: &str) {
    let source = fs::read_to_string(file_path).expect("Failed to read file");
    match run(&source, None) {
        Ok(_) => println!("Ran successfully"),
        Err(errors) => errors.iter().for_each(|e| eprintln!("{e}")),
    }
}

fn run(source: &str, env: Option<Env>) -> Result<Env, Vec<Error>> {
    let lexer = Lexer::new(source);
    trace!("Lexing {source}");
    let tokens = lexer.lex_all_sanitised()?;
    trace!("Parsing {tokens:#?}");
    let parser = Parser::new(&tokens);
    let root = parser.parse_all()?;
    trace!("Interpreting {root:#?}");
    let mut interpreter = Interpreter::new(env);
    match interpreter.interpret_all(root.items) {
        Ok(()) => Ok(interpreter.env),
        Err(e) => Err(vec![e]),
    }
}
