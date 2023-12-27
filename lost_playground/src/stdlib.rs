use lost_compile::{
    environment::Env,
    types::{
        self,
        Type::{self, NativeFunc},
    },
};

use crate::REPL_OUTPUT_VAR;

/// Override the default init_io from stdlib
/// since `println!` cannot be used with wasm
pub fn init(env: &mut Env) {
    // print(args)
    env.set(
        "print".to_string(),
        NativeFunc(types::NativeFunc {
            name: "print".to_string(),
            args: vec!["arg".to_string()],
            body: |interpreter, args| {
                let Type::Str(output) =
                    interpreter.env.borrow().get(REPL_OUTPUT_VAR.to_string())?
                else {
                    unreachable!("The output value cannot be a non-string type");
                };
                interpreter
                    .env
                    .borrow_mut()
                    .assign(
                        REPL_OUTPUT_VAR.to_string(),
                        // The string must be appended to any
                        // output that has not been written yet
                        Type::Str(if output.is_empty() {
                            args[0].to_string()
                        } else {
                            output + "\n" + &args[0].to_string()
                        }),
                    )
                    .expect("no output variable present");
                Ok(Type::Null)
            },
        }),
    );
}
