use crate::builtin::iter_nth;
use crate::scope::Scope;
use crate::types::{EvalResult, SExpr};

use std::cell::RefCell;
use std::rc::Rc;

pub fn eval_sexpr(scope: &Rc<RefCell<Scope>>, sexpr: &SExpr) -> EvalResult {
    use SExpr::*;

    match sexpr {
        Symbol(sym) => {
            if ':' == sym.chars().next().unwrap() {
                return Ok(sexpr.clone());
            }

            if let Some(sexpr) = scope.borrow().resolve(&sym) {
                Ok(sexpr)
            } else {
                Err(format!("{} is undefined", sym))
            }
        }
        Cons(car, cons) => {
            let called = eval_sexpr(scope, &*car)?;
            match called {
                SpecialForm(form) => (form.f)(scope, list_args(&*cons)?),
                BuiltinFn(builtin) => (builtin.f)(eval_args(scope, &*cons)?),
                Closure(_, _, _) => {
                    let args = eval_args(scope, &*cons)?;
                    call_closure(&called, args)
                }
                Vector(_) => {
                    let mut args = vec![called];
                    args.append(&mut eval_args(scope, &*cons)?);
                    iter_nth(args)
                }
                _ => Err(format!("cannot call this")), // TODO: figure out how to get car again
            }
        }
        Vector(v) => {
            let mut els: Vec<SExpr> = vec![];
            for el in v.iter() {
                els.push(eval_sexpr(scope, el)?);
            }
            Ok(SExpr::Vector(Rc::from(els)))
        }
        _ => Ok(sexpr.clone()),
    }
}

pub fn call_callable(callable: &SExpr, args: Vec<SExpr>) -> EvalResult {
    match callable {
        SExpr::Closure(_, _, _) => call_closure(callable, args),
        SExpr::BuiltinFn(builtin) => (builtin.f)(args),
        _ => Err(str!("not callable")),
    }
}

fn call_closure(closure: &SExpr, args: Vec<SExpr>) -> EvalResult {
    if let SExpr::Closure(params, binding, body) = closure {
        let mut args = args;

        loop {
            if args.len() != params.len() {
                return Err(format!(
                    "wrong number of args. Expect {}, got {}",
                    params.len(),
                    args.len()
                ));
            }

            let binding = Scope::unnamed(Some(binding));
            for (param, arg) in params.iter().zip(args) {
                binding.borrow_mut().define(param, arg.clone());
            }

            let result = eval_sexpr(&binding, &**body)?;
            if let SExpr::Recur(rargs) = result {
                args = rargs.clone();
                continue;
            }
            return Ok(result);
        }
    } else {
        unreachable!()
    }
}

fn eval_args(scope: &Rc<RefCell<Scope>>, args: &SExpr) -> Result<Vec<SExpr>, String> {
    let mut v: Vec<SExpr> = vec![];
    for arg in list_args(args)? {
        v.push(eval_sexpr(scope, &arg)?)
    }
    Ok(v)
}

// list_args converts a NIL-terminating cons sequence to a Vec.
fn list_args(args: &SExpr) -> Result<Vec<SExpr>, String> {
    let mut v: Vec<SExpr> = vec![];
    let mut cur = args;
    loop {
        match cur {
            SExpr::Cons(car, cons) => {
                v.push(*car.clone());
                cur = &**cons;
            }
            SExpr::NIL => break,
            _ => panic!("Not a valid arg: {}", cur),
        }
    }
    Ok(v)
}
