use crate::scope::Scope;

use std::cell::RefCell;
use std::fmt::Debug;
use std::rc::Rc;

pub type EvalResult = Result<SExpr, String>;

#[derive(Clone, Debug, PartialEq)]
pub struct BuiltinFn {
    pub name: String,
    pub f: fn(Vec<SExpr>) -> EvalResult,
}

#[derive(Clone)]
pub struct SpecialForm {
    // TODO: Rename to BuiltinSpecial
    pub name: String,
    pub f: fn(&Rc<RefCell<Scope>>, Vec<SExpr>) -> EvalResult,
}

impl Debug for SpecialForm {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "<special {}>", self.name)
    }
}

impl PartialEq for SpecialForm {
    fn eq(&self, o: &SpecialForm) -> bool {
        self.name == o.name // TODO
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum SExpr {
    NIL,
    Undefined,
    Symbol(Rc<str>),
    String(Rc<str>),
    Char(char),
    Integer(i64),
    Ratio(i64, i64),
    Boolean(bool),
    Cons(Box<SExpr>, Rc<SExpr>),
    Vector(Rc<Vec<SExpr>>),

    SpecialForm(Rc<SpecialForm>),
    BuiltinFn(Rc<BuiltinFn>),
    Closure(Rc<Vec<String>>, Rc<RefCell<Scope>>, Rc<SExpr>),

    Recur(Vec<SExpr>), // TODO: this is a hack
}
