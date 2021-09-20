use glob;
use quote::quote;
use regex;
use std::io::Read;
use std::io::Write;

fn main() {
    let out_dir = std::env::var("OUT_DIR").unwrap();
    let dest = std::path::Path::new(&out_dir).join("generated_tests.rs");
    let mut f = std::fs::File::create(&dest).unwrap();

    for entry in glob::glob("src/tests/*.yaml").unwrap() {
        let entry = entry.unwrap();
        let path = entry.as_path();

        for example in read_examples(path) {
            let test_fn = generate_test_fn(path.file_stem().unwrap().to_str().unwrap(), &example);
            write!(f, "{}", test_fn).unwrap();
        }
    }
}

#[derive(PartialEq, serde_derive::Deserialize)]
struct EvalExample {
    name: String,
    expr: String,
    value: serde_yaml::Value,
}

fn read_examples(path: &std::path::Path) -> Vec<EvalExample> {
    let cwd = std::env::current_dir().unwrap();

    let mut file = std::fs::File::open(cwd.join(path)).unwrap();
    let mut contents = String::new();
    file.read_to_string(&mut contents).unwrap();

    serde_yaml::from_str(&contents).unwrap()
}

fn generate_test_fn(name: &str, example: &EvalExample) -> proc_macro2::TokenStream {
    let re = regex::Regex::new(r"[-\s?,]+").unwrap();
    let fn_name = format!(
        "test_example_{}_{}",
        name,
        re.replace_all(&example.name, "_")
    );
    let fn_name = syn::Ident::new(&fn_name, proc_macro2::Span::call_site());

    let expr = syn::LitStr::new(&example.expr, proc_macro2::Span::call_site());
    let expect = tokens_for_value(&example.value);

    quote!(
        #[test]
        fn #fn_name() {
            let scope = builtin::scope();
            assert_eq!(
                read(&#expr.to_owned())
                    .and_then(|sexpr| eval_sexpr(&scope, &sexpr)).unwrap(),
                #expect)
        }
    )
    .into()
}

fn tokens_for_value(val: &serde_yaml::Value) -> proc_macro2::TokenStream {
    use serde_yaml::*;

    match val {
        Value::Null => quote!(SExpr::NIL),
        Value::Bool(true) => quote!(SExpr::Boolean(true)),
        Value::Bool(false) => quote!(SExpr::Boolean(false)),
        Value::Number(n) if n.is_i64() => {
            let n = n.as_i64().unwrap();
            quote!(SExpr::Integer(#n))
        }
        Value::String(s) => {
            let s = syn::LitStr::new(&s, proc_macro2::Span::call_site());

            quote!(read(&#s.to_owned()).unwrap())
        }
        other => panic!("Unknown value {:?}", other),
    }
    .into()
}
