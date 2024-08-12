use std::{cell::RefCell, collections::HashMap, rc::Rc, time::{SystemTime, UNIX_EPOCH}};
use wyhash2::WyHash;

use crate::interpreter::Value;

#[derive(Debug)]
pub struct Environment {
    pub enclosing: Option<Rc<RefCell<Environment>>>,
    values: HashMap<String, Option<Value>, WyHash>
}


impl Environment {
    #[inline(always)]
    pub fn new(enclosing: Option<Rc<RefCell<Environment>>>) -> Self {
        Self {
            enclosing,
            values: HashMap::with_hasher(WyHash::with_seed(594)),
        }
    }

    #[inline(always)]
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
            false
        }
    }

    #[inline(always)]
    pub fn assign_at(&mut self, name: &str, value: Value, dist: usize) -> bool {
        if dist == 0 {
            self.assign(name, value)
        } else {
            self.ancestor(dist).borrow_mut().assign(name, value)
        }
    }
    
    #[inline(always)]
    pub fn get(&self, name: &str) -> Option<Option<Value>> {
        if let Some(value) = self.values.get(name) {
            Some(value.clone())
        } else {
            None
        }

    }

    #[inline(always)]
    pub fn get_at(&self, name: &str, dist: usize) -> Option<Option<Value>> {
        if dist == 0 {
            self.values.get(name).cloned()
        } else {
            self.ancestor(dist).borrow().values.get(name).cloned()
        }
    }

    #[inline(always)]
    pub fn ancestor(&self, dist: usize) -> Rc<RefCell<Environment>> {
        let mut current = self.enclosing.clone();
        
        for _ in 1..dist {
            current = current.unwrap().borrow().enclosing.clone();
        }
        
        current.unwrap()
    }

    #[inline(always)]
    pub fn clear(&mut self) {
        self.values.clear()
    }
}