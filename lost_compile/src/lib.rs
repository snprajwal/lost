pub mod environment;
pub mod error;
pub mod interpret;
pub mod stdlib;
pub mod types;

use crate::error::Exception;
use interpret::Interpreter;
use log::trace;
use lost_syntax::{lex::Lexer, parse::Parser};

pub fn run(source: &str, interpreter: &mut Interpreter) -> Result<(), Vec<Exception>> {
    let lexer = Lexer::new(source);
    trace!("Lexing {source}");
    let tokens = lexer.lex_all_sanitised().map_err(|e| {
        e.into_iter()
            .map(Exception::Error)
            .collect::<Vec<Exception>>()
    })?;
    trace!("Parsing {tokens:#?}");
    let parser = Parser::new(&tokens);
    let root = parser.parse_all().map_err(|e| {
        e.into_iter()
            .map(Exception::Error)
            .collect::<Vec<Exception>>()
    })?;
    trace!("Interpreting {root:#?}");
    match interpreter.interpret_all(root.items) {
        Ok(()) => Ok(()),
        Err(e) => Err(vec![e]),
    }
}
