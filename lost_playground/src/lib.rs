mod stdlib;

use crate::stdlib::init;
use lost_compile::{environment::Env, interpret::Interpreter, resolve::Resolver, run, types::Type};
use std::env;
use wasm_bindgen::prelude::*;

#[cfg(feature = "wee_alloc")]
#[global_allocator]
static ALLOC: wee_alloc::WeeAlloc = wee_alloc::WeeAlloc::INIT;

#[wasm_bindgen]
pub fn init_repl() -> String {
    #[cfg(feature = "console_error_panic_hook")]
    console_error_panic_hook::set_once();

    format!(
        "Lost v{} on {} ({}), Copyright (c) {}",
        env!("CARGO_PKG_VERSION"),
        env::consts::OS,
        env::consts::ARCH,
        // Escape the chevrons for author email
        env!("CARGO_PKG_AUTHORS")
            .replace('<', "&lt;")
            .replace('>', "&gt;"),
    )
}

#[wasm_bindgen]
#[derive(Default)]
pub struct World {
    resolver: Resolver,
    interpreter: Interpreter,
}

pub(crate) const REPL_OUTPUT_VAR: &str = "REPL_OUTPUT";

#[wasm_bindgen]
impl World {
    #[wasm_bindgen(constructor)]
    pub fn new() -> Self {
        let mut env = Env::default();
        // Initialise output variable. This is a hack
        // to print to JS since stdout itself can't be
        // captured and piped into a JS value.
        env.set(REPL_OUTPUT_VAR.to_string(), Type::Str(String::default()));
        // Add stdlib functions
        init(&mut env);

        let interpreter = Interpreter::default();
        interpreter.env.replace(env);

        World {
            resolver: Resolver::new(),
            interpreter,
        }
    }

    pub fn run(&mut self, src: &str) -> Result<String, String> {
        clear_output(&self.interpreter);
        match run(src, &mut self.resolver, &mut self.interpreter) {
            Ok(()) => Ok(self
                .interpreter
                .env
                .borrow()
                .get(REPL_OUTPUT_VAR.to_string())
                .unwrap()
                .to_string()),
            Err(errors) => Err(errors
                .into_iter()
                .fold(String::default(), |a, b| a + &b.to_string() + "\n")
                .trim()
                .to_string()),
        }
    }
}

fn clear_output(interpreter: &Interpreter) {
    interpreter
        .env
        .borrow_mut()
        .assign_at_depth(REPL_OUTPUT_VAR.to_string(), Type::Str(String::default()), 0)
        .expect("no output variable present");
}
