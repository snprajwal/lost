pub mod chunk;
pub mod codegen;
pub mod error;
pub mod vm;

use error::Error;
use log::trace;
use lost_syntax::{lex::Lexer, parse::Parser};
use vm::Vm;

use crate::codegen::Generator;

pub fn run(source: &str, vm: &mut Vm) -> Result<(), Vec<Error>> {
    let lexer = Lexer::new(source);
    trace!("Lexing {source}");
    let tokens = lexer
        .lex_all_sanitised()
        .map_err(|e| e.into_iter().collect::<Vec<_>>())?;
    trace!("Parsing {tokens:#?}");
    let parser = Parser::new(&tokens);
    let root = parser
        .parse()
        .map_err(|e| e.into_iter().collect::<Vec<_>>())?;
    trace!("Generating bytecode for {root:#?}");
    let mut gen = Generator::default();
    let chunk = gen.generate(&root);
    trace!("Executing chunk {chunk:#?}");
    vm.exec(&chunk).map_err(|e| vec![e])
}
