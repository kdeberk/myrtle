use crate::eval::eval_sexpr;
use crate::read;
use crate::scope::Scope;
use std::rc::Rc;

use rustyline::error::ReadlineError;
use rustyline::validate::{ValidationContext, ValidationResult, Validator};
use rustyline::{EditMode, Editor};
use rustyline_derive::{Completer, Helper, Highlighter, Hinter};

#[derive(Debug)]
pub struct Error {
    pub reason: String,
}

pub fn main_loop(scope: Rc<Scope>) -> Result<(), Error> {
    let mut rl = create_line_reader();

    loop {
        match rl.readline("λ ") {
            Ok(line) => {
                match read::read(&line) {
                    Err(str) => println!("{}", str),
                    Ok(sexpr) => {
                        rl.add_history_entry(line.as_str());

                        match eval_sexpr(scope.clone(), sexpr) {
                            Ok(sexpr) => println!("{}", sexpr),
                            Err(str) => println!("error: {}", str),
                        }
                    }
                };
            }
            Err(ReadlineError::Interrupted) => {
                continue; // Ctrl-C just resets the current line
            }
            Err(ReadlineError::Eof) => {
                rl.save_history("/Users/kevindeberk/.cache/myrtle/history.txt")
                    .unwrap();
                return Ok(()); // Ctrl-D closes the app
            }
            Err(err) => {
                return Err(Error {
                    reason: err.to_string(),
                })
            }
        }
    }
}

#[derive(Completer, Helper, Hinter, Highlighter)]
struct ReadLineHelper {}

impl ReadLineHelper {
    fn new() -> ReadLineHelper {
        return ReadLineHelper {};
    }
}

impl Validator for ReadLineHelper {
    fn validate(&self, ctx: &mut ValidationContext) -> rustyline::Result<ValidationResult> {
        Ok(validate_line(ctx.input()))
    }
}

fn create_line_reader() -> Editor<ReadLineHelper> {
    let config = rustyline::Config::builder()
        .history_ignore_space(true)
        .completion_type(rustyline::CompletionType::List)
        .edit_mode(EditMode::Emacs)
        .output_stream(rustyline::OutputStreamType::Stdout)
        .build();

    let h = ReadLineHelper::new();

    let mut rl = Editor::with_config(config);
    rl.set_helper(Some(h));

    let _ = rl.load_history("/Users/kevindeberk/.cache/myrtle/history.txt");

    rl
}

fn validate_line(input: &str) -> ValidationResult {
    let mut stack = vec![];
    let mut escape = false;
    let mut string = false;

    for c in input.chars() {
        if escape {
            escape = false;
            continue;
        }

        match c {
            '(' | '[' if !string => stack.push(c),
            ')' | ']' if !string => match (stack.pop(), c) {
                (Some('('), ')') | (Some('['), ']') => {}
                (Some(wanted), _) => {
                    return ValidationResult::Invalid(Some(format!(
                        "Mismatched brackets: {:?} is not properly closed",
                        wanted
                    )))
                }
                (None, c) => {
                    return ValidationResult::Invalid(Some(format!(
                        "Mismatched brackets: {:?} is unpaired",
                        c
                    )))
                }
            },
            '"' | '\'' => string = !string,
            '\\' => escape = true,
            _ => {}
        }
    }

    if stack.is_empty() {
        ValidationResult::Valid(None)
    } else {
        ValidationResult::Incomplete
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_validate_line() {
        use ValidationResult::*;

        let t = map! {
            "()" => Valid(None),
            "()(" => Incomplete,
            "())" => Invalid(Some("Mismatched brackets: ')' is unpaired".to_string())),
            r#"("()")"# => ValidationResult::Valid(None)
        };
        for (k, v) in t {
            match (validate_line(k), v) {
                (Valid(None), Valid(None)) | (Incomplete, Incomplete) => (),
                (Invalid(Some(s1)), Invalid(Some(s2))) => assert_eq!(s1, s2),
                (_, _) => panic!("Different validation results"),
            }
        }
    }
}
