use lost_compile::{
    environment::Env,
    interpret::Interpreter,
    run,
    types::{
        self,
        Type::{self, NativeFunc},
    },
};
use std::env;
use wasm_bindgen::prelude::*;

#[cfg(feature = "wee_alloc")]
#[global_allocator]
static ALLOC: wee_alloc::WeeAlloc = wee_alloc::WeeAlloc::INIT;

#[wasm_bindgen]
pub fn init() -> String {
    #[cfg(feature = "console_error_panic_hook")]
    console_error_panic_hook::set_once();

    format!(
        "Lost v{} on {} ({}), Copyright (c) {}",
        env!("CARGO_PKG_VERSION"),
        env::consts::OS,
        env::consts::ARCH,
        env!("CARGO_PKG_AUTHORS"),
    )
}

#[wasm_bindgen]
#[derive(Default)]
pub struct World {
    interpreter: Interpreter,
}

const REPL_OUTPUT_VAR: &str = "REPL_OUTPUT";

#[wasm_bindgen]
impl World {
    #[wasm_bindgen(constructor)]
    pub fn new() -> Self {
        let world = Self::default();
        let mut env = Env::default();
        // Initialise output variable. This is a hack
        // to print to JS since stdout itself can't be
        // captured and piped into a JS value.
        env.set(REPL_OUTPUT_VAR.to_string(), Type::Str(String::default()));
        // Add stdlib functions
        init_io(&mut env);
        world.interpreter.env.replace(env);
        world
    }

    pub fn run(&mut self, src: &str) -> Result<String, String> {
        clear_output(&self.interpreter);
        match run(src, &mut self.interpreter) {
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
        .assign(REPL_OUTPUT_VAR.to_string(), Type::Str(String::default()))
        .expect("no output variable present");
}

/// Override the default init_io from stdlib
/// since `println!` cannot be used with wasm
fn init_io(env: &mut Env) {
    // print(args)
    env.set(
        "print".to_string(),
        NativeFunc(types::NativeFunc {
            name: "print".to_string(),
            args: vec!["arg".to_string()],
            body: |interpreter, args| {
                let arg = args.first().expect("argument not present");
                interpreter
                    .env
                    .borrow_mut()
                    .assign(REPL_OUTPUT_VAR.to_string(), Type::Str(arg.to_string()))
                    .expect("no output variable present");
                Ok(Type::Number(arg.to_string().len() as f64))
            },
        }),
    );
}
