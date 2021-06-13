use crate::scope::Scope;
use std::fmt::Debug;
use std::rc::Rc;

#[derive(Clone, Debug, PartialEq)]
pub struct BuiltinFn {
    pub name: String,
    pub f: fn(Vec<SExpr>) -> Result<SExpr, String>,
}
#[derive(Clone, Debug, PartialEq)]
pub struct SpecialForm {
    pub name: String,
    pub f: fn(Rc<Scope>, Vec<SExpr>) -> Result<SExpr, String>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum SExpr {
    NIL,
    Undefined,
    Symbol(String),
    String(String),
    Char(char),
    Integer(i64),
    Ratio(i64, i64),
    Boolean(bool),
    Cons(Box<SExpr>, Box<SExpr>),
    Vector(Vec<SExpr>),

    SpecialForm(SpecialForm),
    BuiltinFn(BuiltinFn),
    Closure(Vec<String>, Rc<Scope>, Box<SExpr>),
}
