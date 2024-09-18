use std::{
    collections::HashMap,
    time::{self, UNIX_EPOCH},
};

use crate::{
    environment::Env,
    types::{NativeFunc, Value},
};

/// Initialises the environment with stdlib functions
/// at the global level for the interpreter.
pub fn init(env: &mut Env) {
    let fns: [(&str, Value); 2] = [
        (
            "print",
            Value::NativeFunc(NativeFunc {
                name: "print".to_string(),
                args: vec!["arg".to_string()],
                body: |_, args| {
                    println!("{}", args[0]);
                    Ok(Value::Null)
                },
            }),
        ),
        (
            "clock",
            Value::NativeFunc(NativeFunc {
                name: "clock".to_string(),
                args: vec![],
                body: |_, _| {
                    Ok(Value::Number(
                        time::SystemTime::now()
                            .duration_since(UNIX_EPOCH)
                            .expect("failed to calculate time")
                            .as_secs_f64()
                            * 1000.0,
                    ))
                },
            }),
        ),
    ];

    fns.into_iter().for_each(|(name, f)| {
        env.set(name, f);
    });
}

/// Initialises the scope with stdlib functions for the resolver
pub fn init_symbols(map: &mut HashMap<String, bool>) {
    const SYMBOLS: [&str; 2] = ["print", "clock"];

    SYMBOLS.into_iter().for_each(|s| {
        map.insert(s.to_string(), true);
    });
}
