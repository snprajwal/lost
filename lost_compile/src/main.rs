use lost_syntax::{
    error::Error,
    lex::Lexer,
    token::{Token, TokenKind},
};
use std::{
    env, fs,
    io::{self, Write},
};

fn main() {
    let args: Vec<String> = env::args().skip(1).collect();
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
    let (stdin, mut stdout) = (io::stdin(), io::stdout());
    let mut code = String::default();
    loop {
        let mut line = String::default();
        print!(">>> ");
        stdout.flush().expect("Failed to flush stdout");
        let n = stdin.read_line(&mut line).expect("Failed to read line");
        // If zero bytes are read, then exit (usually triggered by Ctrl-D)
        if n == 0 {
            break;
        }
        match run(&line) {
            Ok(tokens) => {
                code.push_str(&line);
                tokens
                    .iter()
                    .filter(|t| t.kind != TokenKind::WHITESPACE)
                    .for_each(|t| println!("{t:?}"));
            }
            Err(errors) => errors.iter().for_each(|e| eprintln!("{e}")),
        }
    }
}

fn run_file(file_path: &str) {
    let source = fs::read_to_string(file_path).expect("Failed to read file");
    match run(&source) {
        Ok(tokens) => tokens
            .iter()
            .filter(|t| t.kind != TokenKind::WHITESPACE)
            .for_each(|t| println!("{t:?}")),
        Err(errors) => errors.iter().for_each(|e| eprintln!("{e}")),
    }
}

fn run(source: &str) -> Result<Vec<Token>, Vec<Error>> {
    let mut lexer = Lexer::new(source);
    let mut tokens: Vec<Token> = Vec::default();
    let mut errors: Vec<Error> = Vec::default();
    loop {
        match lexer.lex() {
            Ok(t) => {
                if t.kind == TokenKind::EOF {
                    break;
                } else {
                    tokens.push(t);
                }
            }
            Err(e) => errors.push(e),
        }
    }
    if errors.is_empty() {
        Ok(tokens)
    } else {
        Err(errors)
    }
}
