use crate::types::SExpr;

pub struct Ratio {
    num: i64,
    denom: i64,
    undef: bool,
}

impl Ratio {
    pub fn new(num: i64, denom: i64) -> Self {
        let mut r = Ratio {
            num: num,
            denom: denom,
            undef: false,
        };
        r.normalize();
        r
    }

    pub fn add(&mut self, num: i64, denom: i64) {
        if self.undef {
            return;
        } else if 0 == denom {
            self.undef = true;
            return;
        } else if 1 == denom {
            self.num += num * self.denom;
        } else {
            let cm = lcm(self.denom, denom);
            self.num = self.num * (cm / self.denom) + num * (cm / denom);
            self.denom = cm;
        }
        self.normalize();
    }

    pub fn sub(&mut self, num: i64, denom: i64) {
        self.add(-1 * num, denom);
    }

    pub fn mul(&mut self, num: i64, denom: i64) {
        if self.undef {
            return;
        }
        self.num *= num;
        self.denom *= denom;
        self.normalize();
    }

    pub fn div(&mut self, num: i64, denom: i64) {
        if self.undef {
            return;
        }
        self.num *= denom;
        self.denom *= num;
        self.normalize();
    }

    fn normalize(&mut self) {
        if self.undef {
            return;
        } else if 0 == self.denom {
            self.undef = true
        } else {
            let cd = gcd(self.num, self.denom);
            self.num /= cd;
            self.denom /= cd;
        }
    }

    pub fn as_sexpr(&self) -> SExpr {
        if self.undef {
            return SExpr::Undefined;
        }

        match self.denom {
            0 => SExpr::Undefined,
            1 => SExpr::Integer(self.num),
            n => SExpr::Ratio(self.num, n),
        }
    }
}

// gcd returns the greatest integer that divides both a and b.
fn gcd(a: i64, b: i64) -> i64 {
    if a < b {
        gcd(b, a)
    } else if 0 < b {
        gcd(b, a % b)
    } else {
        a
    }
}

// lcm returns the smallest positive integer that is a product of a and b.
fn lcm(a: i64, b: i64) -> i64 {
    (a * b) / gcd(a, b)
}

#[cfg(tests)]
mod tests {
    use super::*;

    #[test]
    fn test_gcd() {
        let t = map! {
            (1, 1) => 1,
            (10, 1) => 1,
            (5, 2) => 1,
            (10, 2) => 2,
            (2, 10) => 2,
            (12, 18) => 6
        };
        for ((a, b), d) in t {
            assert_eq!(gcd(a, b), d)
        }
    }
}
