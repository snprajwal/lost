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
                println!("{}", args[0]);
                Ok(Type::Null)
            },
        }),
    );
}
