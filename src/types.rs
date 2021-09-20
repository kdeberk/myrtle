use crate::scope::Scope;

use std::cell::RefCell;
use std::fmt::Debug;
use std::rc::Rc;

pub type EvalResult = Result<SExpr, String>;

#[derive(Debug, PartialEq)]
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
    Closure(Rc<Vec<Parameter>>, Rc<RefCell<Scope>>, Rc<SExpr>),

    Recur(Vec<SExpr>), // TODO: this is a hack
}

impl SExpr {
    pub fn name(&self) -> &'static str {
        use SExpr::*;

        match self {
            NIL => "nil",
            Undefined => "undefined",
            Symbol(_) => "symbol",
            String(_) => "string",
            Char(_) => "char",
            Integer(_) => "integer",
            Ratio(_, _) => "ratio",
            Boolean(_) => "boolean",
            Cons(_, _) => "cons",
            Vector(_) => "vector",
            SpecialForm(_) => "specialform",
            BuiltinFn(_) => "builtinfn",
            Closure(_, _, _) => "closure",
            Recur(_) => "recur",
        }
    }
}

#[derive(Debug, PartialEq)]
pub enum Parameter {
    Single(Rc<str>),
    List(Rc<Vec<Parameter>>),
    Rest(Rc<str>),
}

impl Parameter {
    pub fn read_argument(
        &self,
        binding: &Rc<RefCell<Scope>>,
        args: &mut Vec<SExpr>,
    ) -> Result<(), String> {
        use Parameter::*;

        match self {
            Single(name) => {
                if 0 == args.len() {
                    Err(format!("missing argument {}", name))
                } else {
                    binding.borrow_mut().define(name, args.remove(0));
                    Ok(())
                }
            }
            List(params) => {
                if 0 == args.len() {
                    Err(format!("missing argument {:?}", params))
                } else {
                    match args.remove(0) {
                        SExpr::Vector(v) => {
                            let v = &mut (*v).clone();
                            for p in params.iter() {
                                p.read_argument(binding, v)?;
                            }
                        }
                        _ => return Err(str!("cannot destruct vector arg")),
                    }
                    // pop first args, it needs to be a vector

                    Ok(())
                }
            }
            Rest(name) => {
                let mut cur = SExpr::NIL;
                for el in args.iter().rev() {
                    cur = SExpr::Cons(Box::from(el.clone()), Rc::from(cur));
                }
                binding.borrow_mut().define(name, cur);
                Ok(())
            }
        }
    }
}
