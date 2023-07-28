use crate::{
    environment::Env,
    types::{
        self,
        Type::{self, NativeFunc},
    },
};

pub fn init_io(env: &mut Env) {
    // print(args)
    env.set(
        "print".to_string(),
        NativeFunc(types::NativeFunc {
            name: "print".to_string(),
            args: vec!["arg".to_string()],
            body: |_, args| {
                let arg = args.first().expect("argument not present");
                println!("{arg}");
                Ok(Type::Number(arg.to_string().len() as f64))
            },
        }),
    );
}
