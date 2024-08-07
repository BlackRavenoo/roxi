use std::{collections::HashMap, time::{SystemTime, UNIX_EPOCH}};
use wyhash2::WyHash;

use crate::interpreter::Value;

pub struct Environment {
    pub enclosing: Option<Box<Environment>>,
    values: HashMap<String, Option<Value>, WyHash>
}


impl Environment {
    #[inline(always)]
    pub fn new(enclosing: Option<Box<Environment>>) -> Self {
        Self {
            enclosing,
            values: HashMap::with_hasher(WyHash::with_seed(594)),
        }
    }

    #[inline]
    pub fn new_global_env() -> Self {
        let mut env = Environment::new(None);
        env.define(
            "clock",
            Some(Value::NativeFunction {
                arity: 0,
                fun: |_, _| {
                    Value::Number(
                        SystemTime::now()
                            .duration_since(UNIX_EPOCH)
                            .unwrap()
                            .as_secs_f64()  
                    )
                }
            })
        );

        env
    }

    #[inline(always)]
    pub fn define<S: ToString>(&mut self, name: S, value: Option<Value>) {
        self.values.insert(name.to_string(), value);
    }

    #[inline(always)]
    pub fn assign(&mut self, name: &str, value: Value) -> bool {
        if self.values.contains_key(name) {
            self.values.entry(name.to_string()).or_insert(None).replace(value);
            true
        } else {
            self.enclosing
                .as_mut()
                .map_or(false, |env| env.assign(name, value))
        }
    }
    
    #[inline(always)]
    pub fn get(&self, name: &str) -> Option<&Option<Value>> {
        self.values.get(name).or_else(|| {
            self.enclosing
                .as_ref()
                .and_then(|enclosing| enclosing.get(name))
        })
    }
}