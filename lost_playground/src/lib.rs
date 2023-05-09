use std::{env, sync::Mutex};

use lost_compile::{
    environment::Env,
    error::Exception,
    interpret::Interpreter,
    types::{
        self,
        Type::{self, NativeFunc},
    },
};
use lost_syntax::{lex::Lexer, parse::Parser};
use once_cell::sync::Lazy;
use wasm_bindgen::prelude::*;

#[cfg(feature = "wee_alloc")]
#[global_allocator]
static ALLOC: wee_alloc::WeeAlloc = wee_alloc::WeeAlloc::INIT;

#[wasm_bindgen]
pub fn init() -> String {
    #[cfg(feature = "console_error_panic_hook")]
    console_error_panic_hook::set_once();

    format!(
        "Lost REPL v{} on {} ({}), Copyright (c) {}",
        env!("CARGO_PKG_VERSION"),
        env::consts::OS,
        env::consts::ARCH,
        env!("CARGO_PKG_AUTHORS"),
    )
}

struct World {
    env: Env,
    output: String,
}

static WORLD: Lazy<Mutex<World>> = Lazy::new(|| {
    let mut env = Env::default();
    init_io(&mut env);
    Mutex::new(World {
        env,
        output: String::default(),
    })
});

// Override the default init_io from stdlib
// since `println!` cannot be used with wasm
fn init_io(env: &mut Env) {
    // print(args)
    env.set(
        "print".to_string(),
        NativeFunc(types::NativeFunc {
            name: "print".to_string(),
            args: vec!["arg".to_string()],
            body: |_, args| {
                let arg = args.first().expect("Argument not present");
                WORLD.lock().unwrap().output.push_str(&arg.to_string());
                Ok(Type::Number(arg.to_string().len() as f64))
            },
        }),
    );
}

#[wasm_bindgen]
pub fn run_repl(line: &str) -> Result<String, String> {
    let mut code = String::default();
    let mut world = WORLD.lock().unwrap();
    let env = world.env.to_owned();
    // Clear the output buffer
    world.output = String::default();
    // Unlock the mutex since we've cloned the value
    drop(world);
    match run(&line, env) {
        Ok(new_env) => {
            code.push_str(&line);
            let mut world = WORLD.lock().unwrap();
            world.env = new_env;
            Ok(world.output.to_owned())
        }
        Err(errors) => Err(errors
            .into_iter()
            .fold(String::default(), |a, b| a + &b.to_string() + "\n")
            .trim()
            .to_string()),
    }
}

fn run(source: &str, env: Env) -> Result<Env, Vec<Exception>> {
    let lexer = Lexer::new(source);
    let tokens = lexer.lex_all_sanitised().map_err(|e| {
        e.into_iter()
            .map(Exception::Error)
            .collect::<Vec<Exception>>()
    })?;
    let parser = Parser::new(&tokens);
    let root = parser.parse_all().map_err(|e| {
        e.into_iter()
            .map(Exception::Error)
            .collect::<Vec<Exception>>()
    })?;
    let mut interpreter = Interpreter::new(Some(env));
    match interpreter.interpret_all(root.items) {
        Ok(()) => Ok(interpreter.env),
        Err(e) => Err(vec![e]),
    }
}
