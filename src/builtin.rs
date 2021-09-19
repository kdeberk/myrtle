use crate::eval::{call_callable, eval_sexpr};
use crate::read::read;
use crate::scope::Scope;
use crate::types::{BuiltinFn, EvalResult, SExpr, SpecialForm};
use crate::utils::Ratio;

use std::cell::RefCell;
use std::rc::Rc;

// TODO:
// - define consts: e.g. pi / π -> Ratio(1146408/364913)
// - rust macro for checking parameter count, parameter types
// - define defmacro
// - determine what cond and if should when pred returns Undefined

pub fn scope() -> Rc<RefCell<Scope>> {
    let ns = Scope::named(str!("builtin"), None);

    // Special functions
    define_special(&ns, "quote", quote);
    define_special(&ns, "if", if_expr);
    define_special(&ns, "let", let_expr);
    define_special(&ns, "λ", lambda);
    define_special(&ns, "lambda", lambda);
    define_special(&ns, "define", define_expr);
    define_special(&ns, "do", do_expr);
    define_special(&ns, "cond", cond_expr);
    define_special(&ns, "recur", recur_expr);
    define_special(&ns, "eval", eval_expr);

    // Basic functions
    define_function(&ns, "atom?", is_atom);
    define_function(&ns, "nil?", is_nil);
    define_function(&ns, "first", first);
    define_function(&ns, "rest", rest);
    define_function(&ns, "combine", combine);
    define_function(&ns, "list", list);
    define_function(&ns, "read", read_expr);

    // Equality functions
    define_function(&ns, "=", equal_expr);
    define_function(&ns, "<=", less_or_equal_expr);

    // Mathemetical operations
    define_function(&ns, "+", math_add);
    define_function(&ns, "-", math_sub);
    define_function(&ns, "*", math_mul);
    define_function(&ns, "/", math_div);
    define_function(&ns, "mod", math_mod);

    // String functions
    define_function(&ns, "str:chars", str_chars);
    define_function(&ns, "str:trim", str_trim);
    // TODO: split

    // Iter functions
    define_function(&ns, "empty?", empty);
    define_function(&ns, "count", iter_count);
    define_function(&ns, "find", iter_find);
    define_function(&ns, "nth", iter_nth);
    define_function(&ns, "seq", iter_seq);
    define_function(&ns, "reduce", iter_reduce);
    // TODO: map, reduce

    define_function(&ns, "io:read-file", read_file);

    ns
}

fn define_special(
    ns: &Rc<RefCell<Scope>>,
    name: &str,
    special: fn(&Rc<RefCell<Scope>>, Vec<SExpr>) -> EvalResult,
) {
    ns.borrow_mut().define(
        name,
        SExpr::SpecialForm(Rc::from(SpecialForm {
            name: name.to_owned(),
            f: special,
        })),
    );
}

fn quote(_: &Rc<RefCell<Scope>>, args: Vec<SExpr>) -> EvalResult {
    if 1 != args.len() {
        return Err(str!("quote only takes a single argument"));
    }

    Ok(args[0].clone())
}

fn if_expr(scope: &Rc<RefCell<Scope>>, args: Vec<SExpr>) -> EvalResult {
    match args.len() {
        0 | 1 => return Err(str!("if needs at least a condition and a consequence")),
        2 | 3 => (),
        _ => return Err(str!("if only accepts 2 or 3 arguments")),
    }

    match eval_sexpr(scope, &args[0])? {
        SExpr::NIL | SExpr::Boolean(false) => {
            if 0 == args.len() {
                Ok(SExpr::Undefined)
            } else {
                eval_sexpr(scope, &args[2])
            }
        }
        _ => eval_sexpr(scope, &args[1]),
    }
}

fn let_expr(scope: &Rc<RefCell<Scope>>, args: Vec<SExpr>) -> EvalResult {
    if 2 != args.len() {
        return Err(str!("let accepts 2 arguments"));
    }

    let binding = Scope::unnamed(Some(scope));

    match (&args[0], &args[1]) {
        (SExpr::Vector(names_and_exprs), body) => {
            if 0 != names_and_exprs.len() % 2 {
                return Err(str!("let bindings must have an even number"));
            }

            let mut idx = 0;
            while idx < names_and_exprs.len() {
                let name = &names_and_exprs[idx];
                let expr = &names_and_exprs[idx + 1];

                if let SExpr::Symbol(sym) = name {
                    let val = eval_sexpr(&binding, &expr)?;
                    binding.borrow_mut().define(&sym, val);
                } else {
                    return Err(str!("cannot bind value to non-symbol"));
                }
                idx += 2;
            }
            eval_sexpr(&binding, &body)
        }
        _ => Err(str!("let bindings must by a vector")),
    }
}

fn lambda(scope: &Rc<RefCell<Scope>>, args: Vec<SExpr>) -> EvalResult {
    if 2 != args.len() {
        return Err(str!("lambda accepts 2 arguments"));
    }

    match (&args[0], &args[1]) {
        (SExpr::Vector(vec), body) => {
            let mut params: Vec<String> = vec![];

            for p in (&*vec).iter() {
                match p {
                    SExpr::Symbol(sym) => params.push(sym.to_string()),
                    // TODO: allow vectors as params: arguments are destructured during call.
                    _ => return Err(str!("param must be a symbol")),
                }
            }

            Ok(SExpr::Closure(
                Rc::from(params),
                Rc::clone(scope),
                Rc::from(body.clone()),
            ))
        }
        _ => Err(str!("lambda params should be a vector")),
    }
}

fn define_expr(scope: &Rc<RefCell<Scope>>, args: Vec<SExpr>) -> EvalResult {
    if 2 != args.len() {
        return Err(str!("define takes 2 arguments"));
    }

    let name: String = match &args[0] {
        SExpr::Symbol(n) => (*n).to_string(),
        _ => return Err(str!("first argument is a symbol")),
    };

    let val = eval_sexpr(scope, &args[1])?;
    Ok(scope.borrow_mut().define(&name, val))
}

fn do_expr(scope: &Rc<RefCell<Scope>>, args: Vec<SExpr>) -> EvalResult {
    let mut result: SExpr = SExpr::NIL;
    for arg in args {
        result = eval_sexpr(scope, &arg)?;
    }
    Ok(result)
}

fn cond_expr(scope: &Rc<RefCell<Scope>>, args: Vec<SExpr>) -> EvalResult {
    if 0 != args.len() % 2 {
        return Err(str!("cond takes an even number of arguments"));
    }

    let mut idx = 0;
    while idx < args.len() {
        let pred = &args[idx];
        let then = &args[idx + 1];
        match eval_sexpr(scope, pred)? {
            SExpr::Boolean(false) | SExpr::NIL => {}
            _ => return eval_sexpr(scope, then),
        }

        idx += 2;
    }
    Ok(SExpr::Undefined)
}

fn recur_expr(scope: &Rc<RefCell<Scope>>, args: Vec<SExpr>) -> EvalResult {
    let mut eargs: Vec<SExpr> = vec![];
    for el in args.iter() {
        eargs.push(eval_sexpr(scope, el)?)
    }

    Ok(SExpr::Recur(eargs)) // HACK
}

fn eval_expr(scope: &Rc<RefCell<Scope>>, args: Vec<SExpr>) -> EvalResult {
    if 1 != args.len() {
        return Err(str!("eval is defined for a single argument"));
    }
    eval_sexpr(scope, &eval_sexpr(scope, &args[0])?)
}

fn define_function(ns: &Rc<RefCell<Scope>>, name: &str, builtin: fn(Vec<SExpr>) -> EvalResult) {
    ns.borrow_mut().define(
        name.clone(),
        SExpr::BuiltinFn(Rc::from(BuiltinFn {
            name: name.to_owned(),
            f: builtin,
        })),
    );
}

fn is_atom(args: Vec<SExpr>) -> EvalResult {
    if 1 != args.len() {
        return Err(str!("atom? takes a single argument"));
    }

    match &args[0] {
        SExpr::Cons(_, _) => Ok(SExpr::Boolean(false)),
        _ => Ok(SExpr::Boolean(true)),
    }
}

fn is_nil(args: Vec<SExpr>) -> EvalResult {
    if 1 != args.len() {
        return Err(str!("nil? takes a single argument"));
    }

    match &args[0] {
        SExpr::NIL => Ok(SExpr::Boolean(true)),
        _ => Ok(SExpr::Boolean(false)),
    }
}

fn equal_expr(args: Vec<SExpr>) -> EvalResult {
    if args.len() < 2 {
        return Err(str!("= needs at least 2 arguments"));
    }

    let mut cur = &args[0];
    for el in args[1..].iter() {
        match (cur, el) {
            (SExpr::Symbol(a), SExpr::Symbol(b)) => {
                if *a != *b {
                    return Ok(SExpr::Boolean(false));
                }
            }
            (SExpr::Integer(a), SExpr::Integer(b)) => {
                if a != b {
                    return Ok(SExpr::Boolean(false));
                }
            }
            (SExpr::Char(a), SExpr::Char(b)) => {
                if a != b {
                    return Ok(SExpr::Boolean(false));
                }
            }
            _ => return Ok(SExpr::Boolean(false)),
        }
        cur = el;
    }
    Ok(SExpr::Boolean(true))
}

fn less_or_equal_expr(args: Vec<SExpr>) -> EvalResult {
    if args.len() < 2 {
        return Err(str!("<= needs at least 2 arguments"));
    }

    let mut cur = &args[0];
    for el in args[1..].iter() {
        match (cur, el) {
            (SExpr::Integer(a), SExpr::Integer(b)) => {
                if a > b {
                    return Ok(SExpr::Boolean(false));
                }
            }
            _ => return Err(str!("Can only compare integers")),
        }
        cur = el;
    }
    Ok(SExpr::Boolean(true))
}

fn first(args: Vec<SExpr>) -> EvalResult {
    if 1 != args.len() {
        return Err(str!("first takes a single argument"));
    }

    match &args[0] {
        SExpr::Cons(a, _) => Ok(*a.clone()),
        _ => Err(format!("first is only defined for a cons")),
    }
}

fn rest(args: Vec<SExpr>) -> EvalResult {
    if 1 != args.len() {
        return Err(str!("rest takes a single argument"));
    }

    match &args[0] {
        SExpr::Cons(_, a) => Ok((**a).clone()),
        _ => Err(format!("rest is only defined for a cons")),
    }
}

fn combine(args: Vec<SExpr>) -> EvalResult {
    if 2 != args.len() {
        return Err(str!("combine takes two arguments"));
    }

    Ok(SExpr::Cons(
        Box::from((&args[0]).clone()),
        Rc::from((&args[1]).clone()),
    ))
}

fn list(args: Vec<SExpr>) -> EvalResult {
    let mut cur = SExpr::NIL;
    for el in args.iter().rev() {
        cur = SExpr::Cons(Box::new(el.clone()), Rc::from(cur));
    }

    Ok(cur)
}

fn read_expr(args: Vec<SExpr>) -> EvalResult {
    if 1 != args.len() {
        return Err(str!("read takes a single argument"));
    }

    if let SExpr::String(s) = &args[0] {
        Ok(read(&s.to_string())?)
    } else {
        return Err(str!("read takes a single string"));
    }
}

fn math_add(args: Vec<SExpr>) -> EvalResult {
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

fn math_sub(args: Vec<SExpr>) -> EvalResult {
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

fn math_mul(args: Vec<SExpr>) -> EvalResult {
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

fn math_div(args: Vec<SExpr>) -> EvalResult {
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

fn math_mod(args: Vec<SExpr>) -> EvalResult {
    if 2 != args.len() {
        return Err(str!("modulo only takes two arguments"));
    }

    match (&args[0], &args[1]) {
        (SExpr::Integer(a), SExpr::Integer(b)) => Ok(SExpr::Integer(a % b)),
        _ => Err(str!("modulo is only defined for two integers")),
    }
}

fn str_chars(args: Vec<SExpr>) -> EvalResult {
    if 1 != args.len() {
        return Err(str!("chars accepts 1 argument"));
    }

    match &args[0] {
        SExpr::String(s) => {
            let chs: Vec<SExpr> = s.chars().map(|ch| SExpr::Char(ch)).collect();
            Ok(SExpr::Vector(Rc::from(chs)))
        }
        _ => Err(str!("param must be a string")),
    }
}

fn str_trim(args: Vec<SExpr>) -> EvalResult {
    if 1 != args.len() {
        return Err(str!("trim accepts 1 argument"));
    }

    match &args[0] {
        SExpr::String(s) => Ok(SExpr::String(Rc::from(s.trim()))),
        _ => Err(str!("param must be a string")),
    }
}

fn empty(args: Vec<SExpr>) -> EvalResult {
    if 1 != args.len() {
        return Err(str!("empty? accepts a single argument"));
    }

    match &args[0] {
        SExpr::NIL => Ok(SExpr::Boolean(true)),
        SExpr::Cons(_, _) => Ok(SExpr::Boolean(false)),
        SExpr::Vector(v) => Ok(SExpr::Boolean(0 == v.len())),
        _ => Err(str!("empty? is only defined for lists and vector")),
    }
}

fn iter_count(args: Vec<SExpr>) -> EvalResult {
    if 2 != args.len() {
        return Err(str!("count accepts 2 arguments"));
    }

    let pred = &args[0];
    let iter = &args[1];

    let mut cnt = 0;
    let count_ref = &mut cnt;

    match iter {
        SExpr::Cons(_, _) | SExpr::Vector(_) => {
            iter_each_do(&iter, |el: &SExpr| {
                match call_callable(&pred, vec![el.clone()])? {
                    SExpr::Boolean(false) | SExpr::NIL => {}
                    _ => *count_ref += 1,
                }
                Ok(true)
            })?;
            Ok(SExpr::Integer(cnt))
        }
        _ => Err(format!("count only support list and vector")),
    }
}

fn iter_find(args: Vec<SExpr>) -> EvalResult {
    if 2 != args.len() {
        return Err(str!("find accepts 2 arguments"));
    }

    let pred = &args[0];
    let iter = &args[1];

    match iter {
        SExpr::Cons(_, _) | SExpr::Vector(_) => {
            match iter_each_do(&iter, |el: &SExpr| {
                match call_callable(&pred, vec![el.clone()])? {
                    SExpr::Boolean(false) | SExpr::NIL => Ok(true),
                    _ => Ok(false),
                }
            })? {
                Some((idx, found)) => Ok(SExpr::Vector(Rc::from(vec![
                    SExpr::Integer(idx as i64),
                    found,
                ]))),
                None => Ok(SExpr::NIL),
            }
        }
        _ => Err(format!("count only support list and vector")),
    }
}

pub fn iter_nth(args: Vec<SExpr>) -> EvalResult {
    if 2 != args.len() {
        return Err(str!("nth accepts 2 arguments"));
    }

    match (&args[0], &args[1]) {
        (SExpr::Vector(v), SExpr::Integer(idx)) => match v.get(*idx as usize) {
            Some(el) => Ok(el.clone()),
            None => Err(str!("index out of bounds")),
        },
        _ => Err(str!("nth is only defined for vec and integer")),
    }
}

fn iter_seq(args: Vec<SExpr>) -> EvalResult {
    if 1 != args.len() {
        return Err(str!("seq accepts 1 argument"));
    }

    match &args[0] {
        SExpr::Vector(v) => {
            let mut cur = SExpr::NIL;
            for el in v.iter().rev() {
                cur = SExpr::Cons(Box::from(el.clone()), Rc::from(cur));
            }
            Ok(cur)
        }
        _ => Err(str!("seq is only defined for vec")),
    }
}

fn iter_reduce(args: Vec<SExpr>) -> EvalResult {
    if 2 != args.len() {
        return Err(str!("reduce accepts 2 arguments"));
    }

    let pred = &args[0];
    let coll = &args[1];
    match (pred, coll) {
        (SExpr::Closure(_, _, _), SExpr::Cons(car, cons))
        | (SExpr::BuiltinFn(_), SExpr::Cons(car, cons)) => {
            let mut acc: SExpr = *car.clone();
            iter_each_do(cons, |el: &SExpr| {
                acc = call_callable(pred, vec![acc.clone(), el.clone()])?;
                Ok(true)
            })?;
            Ok(acc)
        }
        _ => Err(str!("reduce not supported")),
    }
}

fn iter_each_do<F>(iter: &SExpr, mut f: F) -> Result<Option<(usize, SExpr)>, String>
where
    F: FnMut(&SExpr) -> Result<bool, String>,
{
    match iter {
        SExpr::Cons(_, _) => {
            let mut idx = 0;
            let mut cur = iter;
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
        }
        SExpr::Vector(v) => {
            for (idx, el) in v.iter().enumerate() {
                if !f(el)? {
                    return Ok(Some((idx, el.clone())));
                }
            }
        }
        _ => unreachable!(),
    }
    Ok(None)
}

fn read_file(args: Vec<SExpr>) -> EvalResult {
    if 1 != args.len() {
        return Err(str!("read-file accepts 1 argument"));
    }

    match &args[0] {
        SExpr::String(name) => match std::fs::read_to_string(&**name) {
            Ok(s) => Ok(SExpr::String(Rc::from(s))),
            Err(e) => Err(e.to_string()),
        },
        _ => Err(str!("only accept string")),
    }
}
