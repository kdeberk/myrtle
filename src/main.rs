#[macro_use]
mod common;

mod builtin;
mod eval;
mod print;
mod read;
mod repl;
mod scope;
mod types;
mod utils;

#[macro_use]
extern crate lazy_static;

fn main() {
    let scope = builtin::scope();

    match repl::main_loop(scope) {
        Ok(()) => {}
        Err(err) => {
            println!("Error: {}", err.reason)
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::builtin;
    use crate::eval::eval_sexpr;
    use crate::read::read;
    use crate::types::SExpr;

    // generated_tests.rs is generated by build.rs
    include!(concat!(env!("OUT_DIR"), "/generated_tests.rs"));
}
