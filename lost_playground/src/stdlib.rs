use lost_compile::{
    environment::{
        Env,
        Value::{self, NativeFunc},
    },
    types,
};

use crate::REPL_OUTPUT_VAR;

/// Override the default init_io from stdlib
/// since `println!` cannot be used with wasm
pub fn init(env: &mut Env) {
    // print(args)
    env.set(
        "print",
        NativeFunc(types::NativeFunc {
            name: "print".to_string(),
            args: vec!["arg".to_string()],
            body: |interpreter, args| {
                let Value::Str(output) = interpreter.env.borrow().get(REPL_OUTPUT_VAR)? else {
                    unreachable!("The output value cannot be a non-string type");
                };
                interpreter
                    .env
                    .borrow_mut()
                    .assign(
                        REPL_OUTPUT_VAR,
                        // The string must be appended to any
                        // output that has not been written yet
                        Value::Str(if output.is_empty() {
                            args[0].to_string()
                        } else {
                            output + "\n" + &args[0].to_string()
                        }),
                    )
                    .expect("no output variable present");
                Ok(Value::Null)
            },
        }),
    );
}
