use std::collections::HashMap;
use wyhash2::WyHash;

use crate::interpreter::Value;

pub struct Environment {
    values: HashMap<String, Option<Value>, WyHash>
}

impl Environment {
    #[inline(always)]
    pub fn new() -> Self {
        Self {
            values: HashMap::with_hasher(WyHash::with_seed(0)),
        }
    }

    #[inline]
    pub fn define<S: ToString>(&mut self, name: S, value: Option<Value>) {
        self.values.insert(name.to_string(), value);
    }

    #[inline]
    pub fn assign(&mut self, name: &str, value: Value) -> bool {
        if self.values.contains_key(name) {
            self.values.insert(name.to_string(), Some(value));
            true
        } else {
            false
        }
    }
    
    #[inline]
    pub fn get(&self, name: &str) -> Option<&Option<Value>> {
        self.values.get(name)
    }
}