use crate::types::SExpr;
use crate::utils;
use std::rc::Rc;

// TODO:
// - read cons (foo . bar)
// - reader macros such as quote and backtick

use regex::Regex;

pub fn read(input: &String) -> Result<SExpr, String> {
    let mut input = input.trim();
    let mut sexprs: Vec<SExpr> = vec![];

    while 0 < input.len() {
        let (sexpr, read) = read_sexpr(&input)?;
        sexprs.push(sexpr);
        input = input[read..input.len()].trim();
    }

    match sexprs.len() {
        0 => Err(str!("empty input")),
        1 => Ok(sexprs.remove(0)),
        _ => {
            let mut cur = SExpr::NIL;

            for el in sexprs.into_iter().rev() {
                cur = SExpr::Cons(Box::from(el), Rc::from(cur));
            }

            Ok(SExpr::Cons(
                Box::from(SExpr::Symbol(Rc::from("do"))),
                Rc::from(cur),
            ))
        }
    }
}

fn read_sexpr(s: &str) -> Result<(SExpr, usize), String> {
    match s.chars().next() {
        None => Err(str!("unexpected EOF")),
        Some(c) => match c {
            '(' => read_list(s),
            '[' => read_vector(s),
            '"' => read_string(s),
            '\'' => read_char(s),
            '0'..='9' => read_number(s),
            '-' => read_negative_number(s).or_else(|_| read_symbol(s)),
            c if !c.is_whitespace() => read_symbol(s),
            _ => Err(format!("Unrecognized input {:?}", s)),
        },
    }
}

fn read_list(s: &str) -> Result<(SExpr, usize), String> {
    let (mut seq, idx) = read_sequence(s, ')')?;

    if 0 == seq.len() {
        Ok((SExpr::NIL, idx))
    } else {
        let mut cur: SExpr = SExpr::NIL;

        while let Some(sexpr) = seq.pop() {
            cur = SExpr::Cons(Box::new(sexpr), Rc::new(cur));
        }

        Ok((cur, idx))
    }
}

fn read_vector(s: &str) -> Result<(SExpr, usize), String> {
    let (seq, idx) = read_sequence(s, ']')?;

    let mut vec: Vec<SExpr> = Vec::new();
    for sexpr in seq.into_iter() {
        vec.push(sexpr)
    }

    return Ok((SExpr::Vector(Rc::new(vec)), idx));
}

fn read_sequence(s: &str, until: char) -> Result<(Vec<SExpr>, usize), String> {
    let mut idx = 1;
    let mut seq: Vec<SExpr> = Vec::new();

    'outer: loop {
        let mut idxs = s[idx..].chars();
        // Read whitespace or until we hit the until char
        'inner: while let Some(ch) = idxs.next() {
            match ch {
                c if c == until => {
                    idx += 1;
                    break 'outer;
                }
                c if c.is_whitespace() => {
                    idx += c.len_utf8();
                    continue 'inner;
                }
                _ => break 'inner,
            }
        }

        let (sexpr, read) = read_sexpr(&s[idx..])?;
        seq.push(sexpr);
        idx += read;

        // Read until or mandatory whitespace directly after previous sexpr
        match s[idx..].chars().next() {
            Some(c) if c == until => {
                idx += 1;
                break 'outer;
            }
            Some(c) if c.is_whitespace() => {
                idx += c.len_utf8();
                continue 'outer;
            }
            None => return Err(str!("EOF")),
            _ => return Err(format!("unexpected {}", &s[idx..])),
        }
    }
    return Ok((seq, idx));
}

fn read_number(s: &str) -> Result<(SExpr, usize), String> {
    read_scientific_notation(s)
        .or_else(|_| read_fraction(s))
        .or_else(|_| read_simple_integer(s))
}

fn read_negative_number(s: &str) -> Result<(SExpr, usize), String> {
    if 1 == s.len() || ' ' == s.chars().nth(1).unwrap() {
        return Err("not a number".to_string());
    }

    match read_number(&s[1..])? {
        (SExpr::Integer(n), len) => Ok((SExpr::Integer(-n), len + 1)),
        (SExpr::Ratio(n, d), len) => Ok((SExpr::Ratio(-n, d), len + 1)),
        _ => panic!("expected a number"),
    }
}

fn read_fraction(s: &str) -> Result<(SExpr, usize), String> {
    lazy_static! {
        static ref R: Regex = Regex::new(r"^([[:digit:]]+)/([[:digit:]]+)").unwrap();
    }

    match R.captures(s) {
        None => Err(str!("Invalid number")),
        Some(cap) => {
            let s = cap.get(0).unwrap().as_str();
            let m = cap.get(1).unwrap().as_str();
            let n = cap.get(2).unwrap().as_str();

            match (m.parse::<i64>(), n.parse::<i64>()) {
                (Ok(m), Ok(n)) => Ok((utils::Ratio::new(m, n).as_sexpr(), s.len())),
                (Err(x), _) => Err(x.to_string()),
                (_, Err(x)) => Err(x.to_string()),
            }
        }
    }
}

fn read_scientific_notation(s: &str) -> Result<(SExpr, usize), String> {
    lazy_static! {
        static ref R: Regex = Regex::new(r"^([[:digit:]]+)e([[:digit:]]+)").unwrap();
    }

    match R.captures(s) {
        None => Err(str!("Invalid number")),
        Some(cap) => {
            let s = cap.get(0).unwrap().as_str();
            let m = cap.get(1).unwrap().as_str();
            let n = cap.get(2).unwrap().as_str();

            match (m.parse::<i64>(), n.parse::<i64>()) {
                (Ok(m), Ok(n)) => Ok((SExpr::Integer(m * i64::pow(10, n as u32)), s.len())),
                (Err(x), _) => Err(x.to_string()),
                (_, Err(x)) => Err(x.to_string()),
            }
        }
    }
}

fn read_simple_integer(s: &str) -> Result<(SExpr, usize), String> {
    lazy_static! {
        static ref R: Regex = Regex::new(r"^[[:digit:]]+").unwrap();
    }

    match R.captures(s) {
        None => Err(str!("Invalid number")),
        Some(cap) => {
            let ss = cap.get(0).unwrap().as_str();
            match ss.parse::<i64>() {
                Err(x) => Err(x.to_string()),
                Ok(i) => Ok((SExpr::Integer(i), ss.len())),
            }
        }
    }
}

fn read_string(s: &str) -> Result<(SExpr, usize), String> {
    lazy_static! {
        static ref R: Regex = Regex::new(r#"^"([^"]+)""#).unwrap();
    }

    match R.captures(s) {
        None => Err(str!(format!("invalid string: {}", s))),
        Some(cap) => {
            let ss = cap.get(1).unwrap().as_str();
            Ok((SExpr::String(Rc::from(ss)), 2 + ss.len()))
        }
    }
}

fn read_char(s: &str) -> Result<(SExpr, usize), String> {
    lazy_static! {
        static ref R: Regex = Regex::new(r"^'(.)'").unwrap();
    }

    match R.captures(s) {
        None => Err(str!(format!("invalid char: {}", s))),
        Some(cap) => {
            let ss = cap.get(1).unwrap().as_str();
            Ok((SExpr::Char(ss.chars().next().unwrap()), 2 + ss.len()))
        }
    }
}

fn read_symbol(s: &str) -> Result<(SExpr, usize), String> {
    lazy_static! {
        static ref R: Regex = Regex::new(r#"^[^\s'")\]]+"#).unwrap();
    }

    let sym = R.captures(s).unwrap().get(0).unwrap().as_str();
    match sym.to_lowercase().as_str() {
        "nil" => Ok((SExpr::NIL, sym.len())),
        "true" => Ok((SExpr::Boolean(true), sym.len())),
        "false" => Ok((SExpr::Boolean(false), sym.len())),
        "undefined" => Ok((SExpr::Undefined, sym.len())),
        _ => Ok((SExpr::Symbol(Rc::from(sym)), sym.len())),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_read_sexpr() {
        let t = map! {
            "1" => Ok((SExpr::Integer(1), 1)),
            "[1]" => Ok((SExpr::Vector(Rc::from(vec![SExpr::Integer(1)])), 3))
        };
        for (k, v) in t {
            assert_eq!(read_sexpr(k), v);
        }
    }
    #[test]
    fn test_read_list() {
        let t = map! {
            "()" => Ok((SExpr::NIL, 2)),
            "(1)" => Ok((SExpr::Cons(
                Box::new(SExpr::Integer(1)),
                Rc::new(SExpr::NIL),
            ), 3)),
            r#"(1 "foo")"# => Ok((SExpr::Cons(
                Box::new(SExpr::Integer(1)),
                Rc::new(SExpr::Cons(
                    Box::new(SExpr::String(Rc::from("foo"))),
                    Rc::new(SExpr::NIL),
                )),
            ), 9))
        };
        for (k, v) in t {
            assert_eq!(read_list(k), v);
        }
    }

    #[test]
    fn test_read_vector() {
        let t = map! {
            "[]" => Ok((SExpr::Vector(Rc::from(vec![])), 2)),
            "[1 2 3]" => Ok((SExpr::Vector(Rc::from(vec![
                SExpr::Integer(1),
                SExpr::Integer(2),
                SExpr::Integer(3),
            ])), 7))
        };
        for (k, v) in t {
            assert_eq!(read_vector(k), v);
        }
    }

    #[test]
    fn test_read_sequence() {
        let t = map! {
            "()" => Ok((vec![], 2)),
            r#"(1 "foo" bar)"# => Ok((vec![
                SExpr::Integer(1),
                SExpr::String(Rc::from("foo")),
                SExpr::Symbol(Rc::from("bar"))], 13)),
            "(  1    2  )" => Ok((vec![
                SExpr::Integer(1),
                SExpr::Integer(2)], 12)),
            "(1aa)" => Err(str!("unexpected aa)"))
        };
        for (k, v) in t {
            assert_eq!(read_sequence(k, ')'), v);
        }
    }

    #[test]
    fn test_read_number() {
        let t = map! {
            "1" => Ok((SExpr::Integer(1), 1)),
            "42" => Ok((SExpr::Integer(42), 2))
        };
        for (k, v) in t {
            assert_eq!(read_number(k), v);
        }
    }

    #[test]
    fn test_read_string() {
        let t = map! {
            r#""foo""# => Ok((SExpr::String(Rc::from("foo")), 5)),
            r#""foo bar baz""# => Ok((SExpr::String(Rc::from("foo bar baz")), 13)),
            r#""foo'"# => Err(str!("invalid string: \"foo'")),
            r#""foo"# => Err(str!("invalid string: \"foo"))
        };

        for (k, v) in t {
            assert_eq!(read_string(k), v);
        }
    }

    #[test]
    fn test_read_symbol() {
        let t = map! {
            "foo123" => Ok((SExpr::Symbol(Rc::from("foo123")), 6)),
            "foo-bar" => Ok((SExpr::Symbol(Rc::from("foo-bar")), 7)),
            "nil" => Ok((SExpr::NIL, 3)),
            "NiL" => Ok((SExpr::NIL, 3))
        };
        for (k, v) in t {
            assert_eq!(read_symbol(k), v);
        }
    }
}
