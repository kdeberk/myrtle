use crate::types::SExpr;

use std::fmt;

impl fmt::Display for SExpr {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            SExpr::NIL => write!(f, "nil")?,
            SExpr::Undefined => write!(f, "undefined")?,
            SExpr::Symbol(s) => write!(f, "{}", s)?,
            SExpr::Integer(n) => write!(f, "{}", n)?,
            SExpr::Ratio(n, d) => write!(f, "{}/{}", n, d)?,
            SExpr::Boolean(b) => match b {
                true => write!(f, "true")?,
                false => write!(f, "false")?,
            },
            SExpr::String(s) => write!(f, "\"{}\"", s)?,
            SExpr::Char(ch) => write!(f, "'{}'", ch)?,
            SExpr::Cons(_, _) => {
                write!(f, "(")?;
                let mut cur = self;
                let mut cnt = 0;
                while let SExpr::Cons(car, cons) = cur {
                    if 0 == cnt {
                        write!(f, "{}", *car)?;
                    } else {
                        write!(f, " {}", *car)?;
                    }
                    cur = cons;
                    cnt += 1;
                }
                match cur {
                    SExpr::NIL => (),
                    _ => write!(f, " . {}", cur)?,
                }
                write!(f, ")")?
            }
            SExpr::BuiltinFn(builtin) => write!(f, "<builtin {}>", builtin.name)?,
            SExpr::Vector(vec) => {
                write!(f, "[")?;
                for (i, el) in vec.iter().enumerate() {
                    write!(f, "{}", el)?;
                    if i < vec.len() - 1 {
                        write!(f, " ")?;
                    }
                }
                write!(f, "]")?
            }
            SExpr::Map(m) => {
                write!(f, "{{")?;
                for (i, (key, val)) in m.borrow().map.iter().enumerate() {
                    write!(f, "<")?;
                    write!(f, "{} ", key)?;
                    write!(f, "{}", val)?;
                    write!(f, ">")?;
                    if i < m.borrow().map.len() - 1 {
                        write!(f, " ")?;
                    }
                }
                write!(f, "}}")?;
            }
            SExpr::Closure(params, _, body) => {
                write!(f, "<closure ")?;
                for el in params.iter() {
                    write!(f, "{:?} ", el)?;
                }
                write!(f, "{}>", body)?
            }
            SExpr::SpecialForm(form) => write!(f, "<special {}>", form.name)?,
            SExpr::Recur(_) => write!(f, "<recur>")?,
        };
        Ok(())
    }
}
