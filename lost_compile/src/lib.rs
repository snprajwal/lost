pub mod environment;
pub mod error;
pub mod interpret;
pub mod resolve;
pub mod stdlib;
pub mod types;

use crate::{error::Exception, resolve::Resolver};
use interpret::Interpreter;
use log::trace;
use lost_syntax::{lex::Lexer, parse::Parser};

pub fn run(
    source: &str,
    resolver: &mut Resolver,
    interpreter: &mut Interpreter,
) -> Result<(), Vec<Exception>> {
    let lexer = Lexer::new(source);
    trace!("Lexing {source}");
    let tokens = lexer.lex_all_sanitised().map_err(|e| {
        e.into_iter()
            .map(Exception::Error)
            .collect::<Vec<Exception>>()
    })?;
    trace!("Parsing {tokens:#?}");
    let parser = Parser::new(&tokens);
    let root = parser.parse().map_err(|e| {
        e.into_iter()
            .map(Exception::Error)
            .collect::<Vec<Exception>>()
    })?;
    trace!("Resolving {root:#?}");
    let depths = resolver.resolve(root.clone()).map_err(|e| vec![e])?;
    trace!("Interpreting with depths {depths:?}");
    interpreter.interpret(root, depths).map_err(|e| vec![e])
}
