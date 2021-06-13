use crate::scope::Scope;
use crate::types::{BuiltinFn, SExpr, SpecialForm};
use std::rc::Rc;

type EvalResult = Result<SExpr, String>;

pub fn eval_sexpr(scope: Rc<Scope>, sexpr: SExpr) -> EvalResult {
    match sexpr {
        SExpr::Symbol(sym) => {
            if let Some(sexpr) = scope.resolve(&sym) {
                Ok((*sexpr).clone())
            } else {
                Err(format!("{} is undefined", sym))
            }
        }
        SExpr::Cons(car, cons) => {
            let called = eval_sexpr(scope.clone(), *car)?;
            match called {
                SExpr::SpecialForm(SpecialForm { name: _, f }) => f(scope, list_args(*cons)?),
                SExpr::BuiltinFn(BuiltinFn { name: _, f }) => f(eval_args(scope.clone(), *cons)?),
                SExpr::Closure(_, _, _) => {
                    let args = eval_args(scope.clone(), *cons)?;
                    call_closure(&called, args)
                }
                _ => Err(format!("cannot call this")), // TODO: figure out how to get car again
            }
        }
        _ => Ok(sexpr),
    }
}

pub fn call_closure(closure: &SExpr, args: Vec<SExpr>) -> EvalResult {
    if let SExpr::Closure(params, binding, body) = closure {
        if args.len() != params.len() {
            return Err(format!(
                "wrong number of args. Expect {}, got {}",
                params.len(),
                args.len()
            ));
        }

        let mut binding = Scope::unnamed(Some(binding.clone()));
        for (param, arg) in params.iter().zip(args) {
            binding.define(param, arg);
        }
        eval_sexpr(Rc::new(binding), (**body).clone())
    } else {
        unreachable!()
    }
}

fn eval_args(scope: Rc<Scope>, args: SExpr) -> Result<Vec<SExpr>, String> {
    let mut v = Vec::new();
    for arg in list_args(args)? {
        v.push(eval_sexpr(scope.clone(), arg)?);
    }
    Ok(v)
}

// list_args converts a Cons object with function/special/macro arguments to a Vec.
fn list_args(args: SExpr) -> Result<Vec<SExpr>, String> {
    let mut v = Vec::new();
    let mut cur = args;
    loop {
        match cur {
            SExpr::Cons(car, cons) => {
                v.push(*car);
                cur = *cons;
            }
            SExpr::NIL => break,
            _ => panic!("Not a valid arg: {}", cur),
        }
    }
    Ok(v)
}
