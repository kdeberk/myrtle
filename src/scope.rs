use crate::types::SExpr;
use std::collections::HashMap;
use std::rc::Rc;

#[derive(Debug, PartialEq)]
pub struct Scope {
    name: String,
    parent: Option<Rc<Scope>>,
    values: HashMap<String, Rc<SExpr>>,
}

impl Scope {
    pub fn named(name: String, parent: Option<Rc<Scope>>) -> Self {
        Scope {
            name: name,
            parent: parent,
            values: HashMap::new(),
        }
    }

    pub fn unnamed(parent: Option<Rc<Scope>>) -> Self {
        Scope {
            name: String::new(),
            parent: parent,
            values: HashMap::new(),
        }
    }

    pub fn define(&mut self, name: &str, value: SExpr) -> SExpr {
        self.values
            .insert(String::from(name), Rc::new(value.clone()));
        value
    }

    pub fn resolve(&self, name: &str) -> Option<Rc<SExpr>> {
        if let Some(val) = self.values.get(name) {
            Some(Rc::clone(val))
        } else if let Some(p) = &self.parent {
            p.resolve(name)
        } else {
            None
        }
    }
}
