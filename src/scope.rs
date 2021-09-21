use crate::types::SExpr;
use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

#[derive(Debug)]
pub struct Scope {
    name: String,
    parent: Option<Rc<RefCell<Scope>>>,
    values: HashMap<String, SExpr>,
}

// A scope is a collection of named (= symbols) to SExpr values.  Unnamed scopes are
// created for every let and lambda action and the repl starts with the builtin scope that
// defines the builtin constants and functions.  A scope combines the features of a named
// namespace and the anonymous binding created for each let and closure.
impl Scope {
    fn new(name: String, parent: Option<&Rc<RefCell<Scope>>>) -> Rc<RefCell<Self>> {
        let mut scope = Scope {
            name: name,
            parent: None,
            values: HashMap::new(),
        };

        if let Some(parent) = parent {
            scope.parent = Some(Rc::clone(&*parent));
        }

        Rc::from(RefCell::from(scope))
    }

    // named returns a named scope.
    pub fn named(name: String, parent: Option<&Rc<RefCell<Scope>>>) -> Rc<RefCell<Self>> {
        Self::new(name, parent)
    }

    // unnamed returns an anonymous scope.
    pub fn unnamed(parent: Option<&Rc<RefCell<Scope>>>) -> Rc<RefCell<Self>> {
        Self::new(String::new(), parent)
    }

    // define sets a new value or overrides the previous value for the given name.
    pub fn define(&mut self, name: &str, value: SExpr) -> SExpr {
        self.values.insert(name.to_owned(), value.clone());
        value
    }

    // resolve searches for the SExpr associated with the symbol in the current and parent
    // scopes.
    pub fn resolve(&self, name: &str) -> Option<SExpr> {
        if let Some(val) = self.values.get(name) {
            Some(val.clone())
        } else if let Some(p) = &self.parent {
            p.borrow().resolve(name)
        } else {
            None
        }
    }
}
