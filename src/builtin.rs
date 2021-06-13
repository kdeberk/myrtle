use crate::eval::{call_closure, eval_sexpr};
use crate::scope::Scope;
use crate::types::{BuiltinFn, SExpr, SpecialForm};
use crate::utils::Ratio;

use std::rc::Rc;

// TODO:
//  define consts: e.g. pi / π -> Ratio(1146408/364913)
//  util macro for checking parameter count, parameter types

pub fn scope() -> Rc<Scope> {
    let mut ns = Scope::named(str!("builtin"), None);
    // Special functions
    define_special(&mut ns, "quote", quote);
    define_special(&mut ns, "if", if_expr);
    define_special(&mut ns, "let", let_expr);
    define_special(&mut ns, "λ", lambda);
    define_special(&mut ns, "lambda", lambda);
    define_special(&mut ns, "define", define);

    // Basic functions
    define_function(&mut ns, "atom?", is_atom);
    define_function(&mut ns, "nil?", is_nil);
    define_function(&mut ns, "=", equal_symbol);
    define_function(&mut ns, "first", first);
    define_function(&mut ns, "rest", rest);
    define_function(&mut ns, "combine", combine);
    define_function(&mut ns, "list", list);

    // Mathemetical operations
    define_function(&mut ns, "+", math_add);
    define_function(&mut ns, "-", math_sub);
    define_function(&mut ns, "*", math_mul);
    define_function(&mut ns, "/", math_div);
    define_function(&mut ns, "mod", math_mod);

    // String operations
    define_function(&mut ns, "chars", str_chars);

    // Iter operations
    define_function(&mut ns, "count", iter_count);
    define_function(&mut ns, "find", iter_find);

    Rc::new(ns)
}

pub fn define_function(
    ns: &mut Scope,
    name: &str,
    builtin: fn(Vec<SExpr>) -> Result<SExpr, String>,
) {
    ns.define(
        name.clone(),
        SExpr::BuiltinFn(BuiltinFn {
            name: name.to_owned(),
            f: builtin,
        }),
    );
}

pub fn define_special(
    ns: &mut Scope,
    name: &str,
    special: fn(Rc<Scope>, Vec<SExpr>) -> Result<SExpr, String>,
) {
    ns.define(
        name,
        SExpr::SpecialForm(SpecialForm {
            name: name.to_owned(),
            f: special,
        }),
    );
}

pub fn is_atom(mut args: Vec<SExpr>) -> Result<SExpr, String> {
    if 1 != args.len() {
        return Err(str!("atom? takes a single argument"));
    }

    match args.remove(0) {
        SExpr::Cons(_, _) => Ok(SExpr::Boolean(false)),
        _ => Ok(SExpr::Boolean(true)),
    }
}

pub fn is_nil(mut args: Vec<SExpr>) -> Result<SExpr, String> {
    if 1 != args.len() {
        return Err(str!("nil? takes a single argument"));
    }

    match args.remove(0) {
        SExpr::NIL => Ok(SExpr::Boolean(true)),
        _ => Ok(SExpr::Boolean(false)),
    }
}

pub fn equal_symbol(mut args: Vec<SExpr>) -> Result<SExpr, String> {
    if args.len() < 2 {
        return Err(str!("= needs at least 2 arguments"));
    }

    let a = args.remove(0);
    let b = args.remove(0);
    match (a, b) {
        (SExpr::Symbol(a), SExpr::Symbol(b)) => Ok(SExpr::Boolean(a == b)),
        (SExpr::Integer(a), SExpr::Integer(b)) => Ok(SExpr::Boolean(a == b)),
        (SExpr::Char(a), SExpr::Char(b)) => Ok(SExpr::Boolean(a == b)),
        _ => Ok(SExpr::Boolean(false)),
    }
}

pub fn first(mut args: Vec<SExpr>) -> Result<SExpr, String> {
    if 1 != args.len() {
        return Err(str!("first takes a single argument"));
    }

    match args.remove(0) {
        SExpr::Cons(a, _) => Ok(*a),
        _ => Err(format!("first is only defined for a cons")),
    }
}

pub fn rest(mut args: Vec<SExpr>) -> Result<SExpr, String> {
    if 1 != args.len() {
        return Err(str!("rest takes a single argument"));
    }

    match args.remove(0) {
        SExpr::Cons(_, a) => Ok(*a),
        _ => Err(format!("rest is only defined for a cons")),
    }
}

pub fn combine(mut args: Vec<SExpr>) -> Result<SExpr, String> {
    if 2 != args.len() {
        return Err(str!("combine takes two arguments"));
    }

    let a = args.remove(0);
    let b = args.remove(0);

    Ok(SExpr::Cons(Box::new(a), Box::new(b)))
}

pub fn math_add(args: Vec<SExpr>) -> Result<SExpr, String> {
    let mut r = Ratio::new(0, 1);

    for arg in args.iter() {
        match arg {
            SExpr::Integer(n) => r.add(*n, 1),
            SExpr::Ratio(n, d) => r.add(*n, *d),
            SExpr::Undefined => return Ok(SExpr::Undefined),
            other => {
                return Err(format!("addition is not defined for {}", other));
            }
        }
    }
    Ok(r.as_sexpr())
}

pub fn math_sub(args: Vec<SExpr>) -> Result<SExpr, String> {
    let mut cnt = 0;
    let mut r = Ratio::new(0, 1);
    for arg in args.iter() {
        match (cnt, arg) {
            (0, SExpr::Integer(n)) => {
                r = Ratio::new(*n, 1);
            }
            (0, SExpr::Ratio(n, d)) => {
                r = Ratio::new(*n, *d);
            }
            (_, SExpr::Integer(n)) => r.sub(*n, 1),
            (_, SExpr::Ratio(n, d)) => r.sub(*n, *d),
            (_, SExpr::Undefined) => return Ok(SExpr::Undefined),
            (_, other) => {
                return Err(format!("subtraction is not defined for {}", other));
            }
        }
        cnt += 1;
    }

    let res = r.as_sexpr();
    match (cnt, res) {
        (1, SExpr::Ratio(n, d)) => Ok(SExpr::Ratio(-n, d)),
        (1, SExpr::Integer(n)) => Ok(SExpr::Integer(-n)),
        (_, any) => Ok(any),
    }
}

pub fn math_mul(args: Vec<SExpr>) -> Result<SExpr, String> {
    if args.is_empty() {
        return Err(str!("wrong number of args: 0"));
    }

    let mut r = Ratio::new(1, 1);
    for arg in args.iter() {
        match arg {
            SExpr::Undefined => return Ok(SExpr::Undefined),
            SExpr::Integer(n) => r.mul(*n, 1),
            SExpr::Ratio(n, d) => r.mul(*n, *d),
            other => return Err(format!("multiplication is not defined for {}", other)),
        }
    }
    Ok(r.as_sexpr())
}

pub fn math_div(args: Vec<SExpr>) -> Result<SExpr, String> {
    let mut r = Ratio::new(1, 1);
    let mut cnt = 0;
    for arg in args.iter() {
        match (cnt, arg) {
            (_, SExpr::Undefined) => return Ok(SExpr::Undefined),
            (0, SExpr::Integer(i)) => r = Ratio::new(*i, 1),
            (0, SExpr::Ratio(n, d)) => r = Ratio::new(*n, *d),
            (_, SExpr::Integer(0)) => return Ok(SExpr::Undefined),
            (_, SExpr::Integer(i)) => r.div(*i, 1),
            (_, SExpr::Ratio(n, d)) => r.div(*n, *d),
            (_, other) => {
                return Err(format!("division is not defined for {}", other));
            }
        }
        cnt += 1;
    }
    Ok(r.as_sexpr())
}

pub fn math_mod(mut args: Vec<SExpr>) -> Result<SExpr, String> {
    if 2 != args.len() {
        return Err(str!("modulo only takes two arguments"));
    }
    let a = args.remove(0);
    let b = args.remove(0);
    match (a, b) {
        (SExpr::Integer(a), SExpr::Integer(b)) => Ok(SExpr::Integer(a % b)),
        _ => Err(str!("modulo is only defined for two integers")),
    }
}

pub fn list(mut args: Vec<SExpr>) -> Result<SExpr, String> {
    let mut cur = SExpr::NIL;
    while let Some(val) = args.pop() {
        cur = SExpr::Cons(Box::new(val), Box::new(cur));
    }

    Ok(cur)
}

pub fn quote(_: Rc<Scope>, mut args: Vec<SExpr>) -> Result<SExpr, String> {
    if 1 != args.len() {
        return Err(str!("quote only takes a single argument"));
    }

    Ok(args.remove(0))
}

pub fn if_expr(scope: Rc<Scope>, mut args: Vec<SExpr>) -> Result<SExpr, String> {
    match args.len() {
        0 | 1 => return Err(str!("if needs at least a condition and a consequence")),
        2 | 3 => (),
        _ => return Err(str!("if only accepts 2 or 3 arguments")),
    }

    let cond = args.remove(0);
    let then = args.remove(0);
    match eval_sexpr(scope.clone(), cond)? {
        SExpr::NIL | SExpr::Boolean(false) => {
            if 0 == args.len() {
                Ok(SExpr::Undefined)
            } else {
                let alt = args.remove(0);
                eval_sexpr(scope.clone(), alt)
            }
        }
        _ => eval_sexpr(scope.clone(), then),
    }
}

pub fn let_expr(scope: Rc<Scope>, mut args: Vec<SExpr>) -> Result<SExpr, String> {
    if 2 != args.len() {
        return Err(str!("let accepts 2 arguments"));
    }

    let mut binding = Scope::unnamed(Some(scope.clone()));
    match args.remove(0) {
        SExpr::Vector(mut names_and_exprs) => {
            if 0 != names_and_exprs.len() % 2 {
                return Err(str!("let bindings must have an even number"));
            }

            while 0 < names_and_exprs.len() {
                let name = names_and_exprs.remove(0);
                let expr = names_and_exprs.remove(0);

                match name {
                    SExpr::Symbol(sym) => {
                        binding.define(&sym, eval_sexpr(scope.clone(), expr)?);
                    }
                    // TODO: allow vector
                    _ => return Err(str!("cannot bind value to non-symbol")),
                }
            }
            let bodyexpr = args.remove(0);
            eval_sexpr(Rc::new(binding), bodyexpr)
        }
        _ => Err(str!("let bindings must by a vector")),
    }
}

pub fn lambda(scope: Rc<Scope>, mut args: Vec<SExpr>) -> Result<SExpr, String> {
    if 2 != args.len() {
        return Err(str!("lambda accepts 2 arguments"));
    }

    match args.remove(0) {
        SExpr::Vector(vec) => {
            let mut params = Vec::new();

            for p in vec {
                match p {
                    SExpr::Symbol(sym) => params.push(sym),
                    // TODO: allow vectors as params: arguments are destructured during call.
                    _ => return Err(str!("param must be a symbol")),
                }
            }

            Ok(SExpr::Closure(
                params,
                scope.clone(),
                Box::new(args.remove(0)),
            ))
        }
        _ => Err(str!("lambda params should be a vector")),
    }
}

pub fn str_chars(mut args: Vec<SExpr>) -> Result<SExpr, String> {
    if 1 != args.len() {
        return Err(str!("chars accepts 1 argument"));
    }

    match args.remove(0) {
        SExpr::String(s) => {
            let chs: Vec<SExpr> = s.chars().map(|ch| SExpr::Char(ch)).collect();
            Ok(SExpr::Vector(chs))
        }
        _ => Err(str!("param must be a string")),
    }
}

pub fn iter_count(mut args: Vec<SExpr>) -> Result<SExpr, String> {
    if 2 != args.len() {
        return Err(str!("count accepts 2 arguments"));
    }

    let pred = args.remove(0);
    let iter = args.remove(0);

    let mut cnt = 0;
    let count_ref = &mut cnt;

    match iter {
        SExpr::Cons(_, _) => {
            cons_each_do(&iter, |el: &SExpr| {
                match call_closure(&pred, vec![el.clone()])? {
                    SExpr::Boolean(false) | SExpr::NIL => {}
                    _ => *count_ref += 1,
                }
                Ok(true)
            })?;
            Ok(SExpr::Integer(cnt))
        }
        SExpr::Vector(v) => {
            for el in v {
                if let SExpr::Boolean(true) = call_closure(&pred, vec![el.clone()]).unwrap() {
                    *count_ref += 1
                }
            }
            Ok(SExpr::Integer(cnt))
        }
        _ => Err(format!("count only support list and vector")),
    }
}

pub fn iter_find(mut args: Vec<SExpr>) -> Result<SExpr, String> {
    if 2 != args.len() {
        return Err(str!("find accepts 2 arguments"));
    }

    let pred = args.remove(0);
    let iter = args.remove(0);

    match iter {
        SExpr::Cons(_, _) => {
            match cons_each_do(&iter, |el: &SExpr| {
                match call_closure(&pred, vec![el.clone()])? {
                    SExpr::Boolean(false) | SExpr::NIL => Ok(true),
                    _ => Ok(false),
                }
            })? {
                Some((idx, found)) => Ok(SExpr::Vector(vec![SExpr::Integer(idx as i64), found])),
                _ => Ok(SExpr::NIL),
            }
        }
        SExpr::Vector(v) => {
            for (idx, el) in v.iter().enumerate() {
                match call_closure(&pred, vec![el.clone()])? {
                    SExpr::Boolean(false) | SExpr::NIL => {}
                    _ => return Ok(SExpr::Vector(vec![SExpr::Integer(idx as i64), el.clone()])),
                }
            }
            Ok(SExpr::NIL)
        }
        _ => Err(format!("count only support list and vector")),
    }
}

pub fn cons_each_do<F>(cons: &SExpr, mut f: F) -> Result<Option<(usize, SExpr)>, String>
where
    F: FnMut(&SExpr) -> Result<bool, String>,
{
    let mut idx = 0;
    let mut cur = cons;
    while let SExpr::Cons(car, cons) = cur {
        if !f(&car)? {
            return Ok(Some((idx, (**car).clone())));
        }

        match &**cons {
            SExpr::Cons(_, _) => cur = &cons,
            SExpr::NIL => break,
            other => {
                if !f(other)? {
                    return Ok(Some((idx, (**cons).clone())));
                }
            }
        }
        idx += 1
    }
    Ok(None)
}

pub fn define(mut scope: Rc<Scope>, mut args: Vec<SExpr>) -> Result<SExpr, String> {
    if 2 != args.len() {
        return Err(str!("define takes 2 arguments"));
    }

    let name: String = match args.remove(0) {
        SExpr::Symbol(n) => n.clone(),
        _ => return Err(str!("first argument is a symbol")),
    };

    let value = eval_sexpr(scope.clone(), args.remove(0))?;
    Ok(Rc::get_mut(&mut scope).unwrap().define(&name, value))
}
