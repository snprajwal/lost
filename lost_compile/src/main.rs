use lost_syntax::{
    ast::AstNode,
    error::Error,
    lex::Lexer,
    parse::Parser,
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
            Ok(node) => {
                code.push_str(&line);
                println!("{node:#?}");
            }
            Err(errors) => errors.iter().for_each(|e| eprintln!("{e}")),
        }
    }
}

fn run_file(file_path: &str) {
    let source = fs::read_to_string(file_path).expect("Failed to read file");
    match run(&source) {
        Ok(node) => println!("{node:#?}"),
        Err(errors) => errors.iter().for_each(|e| eprintln!("{e}")),
    }
}

fn run(source: &str) -> Result<AstNode, Vec<Error>> {
    let lexer = Lexer::new(source);
    let mut tokens: Vec<Token> = Vec::default();
    let mut errors: Vec<Error> = Vec::default();
    match lexer.lex_all() {
        Ok(mut t) => tokens.append(&mut t),
        Err(mut e) => errors.append(&mut e),
    }

    if !errors.is_empty() {
        return Err(errors);
    }

    let sanitised_tokens = tokens
        .into_iter()
        .filter(|t| !matches!(t.kind, TokenKind::WHITESPACE | TokenKind::COMMENT))
        .collect::<Vec<Token>>();
    let parser = Parser::new(&sanitised_tokens);
    parser.parse().map_err(|e| {
        errors.push(e);
        errors
    })
}
