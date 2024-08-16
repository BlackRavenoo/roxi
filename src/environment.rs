use std::{cell::RefCell, collections::HashMap, rc::Rc, time::{SystemTime, UNIX_EPOCH}};
use wyhash2::WyHash;

use crate::{interpreter::Value, resolver::Binding};

pub struct GlobalEnvironment {
    values: HashMap<String, Option<Rc<RefCell<Value>>>, WyHash>
}

impl GlobalEnvironment {
    #[inline(always)]
    pub fn new() -> Self {
        Self {
            values: HashMap::with_capacity_and_hasher(4, WyHash::with_seed(0))
        }
    }

    #[inline(always)]
    pub fn new_global_env() -> Self {
        let mut env = GlobalEnvironment::new();
        env.define(
            "clock",
            Some(Rc::new(RefCell::new(Value::NativeFunction {
                arity: 0,
                fun: |_, _| {
                    Value::Number(
                        SystemTime::now()
                            .duration_since(UNIX_EPOCH)
                            .unwrap()
                            .as_secs_f64()
                    )
                }
            })))
        );

        env
    }

    #[inline(always)]
    pub fn define<S: ToString>(&mut self, name: S, value: Option<Rc<RefCell<Value>>>) {
        self.values.insert(name.to_string(), value);
    }

    #[inline(always)]
    pub fn assign(&mut self, name: &str, value: Option<Rc<RefCell<Value>>>) -> bool {
        if let Some(var) = self.values.get_mut(name) {
            *var = value;
            true
        } else {
            false
        }
    }
    
    #[inline(always)]
    pub fn get(&self, name: &str) -> Option<Option<Rc<RefCell<Value>>>> {
        self.values.get(name).cloned()
    }
}

#[derive(Debug)]
pub struct LocalEnvironment {
    pub enclosing: Option<Rc<RefCell<LocalEnvironment>>>,
    values: Vec<Option<Rc<RefCell<Value>>>>,
}

impl LocalEnvironment {
    #[inline(always)]
    pub fn new(enclosing: Option<Rc<RefCell<LocalEnvironment>>>) -> Self {
        Self {
            enclosing,
            values: Vec::with_capacity(4),
        }
    }

    #[inline(always)]
    pub fn assign(&mut self, binding: &Binding, value: Option<Rc<RefCell<Value>>>) {
        if self.values.capacity() <= binding.index {
            self.values.reserve(1 + binding.index - self.values.capacity());
        }

        while self.values.len() <= binding.index {
            self.values.push(None)
        }

        self.values[binding.index] = value;
    }

    #[inline(always)]
    pub fn assign_at(&mut self, binding: &Binding, value: Option<Rc<RefCell<Value>>>) {
        if binding.scopes_up == 0 {
            self.assign(binding, value)
        } else {
            self.ancestor(binding.index).borrow_mut().assign(binding, value)
        }
    }

    #[inline(always)]
    pub fn get_at(&self, binding: &Binding) -> Option<Option<Rc<RefCell<Value>>>> {
        if binding.scopes_up == 0 {
            self.values.get(binding.index).cloned()
        } else {
            self.ancestor(binding.scopes_up).borrow().values.get(binding.index).cloned()
        }
    }

    #[inline(always)]
    pub fn ancestor(&self, dist: usize) -> Rc<RefCell<LocalEnvironment>> {
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