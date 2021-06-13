macro_rules! str(
    ($s:expr) => ( $s.to_owned());
);

#[allow(unused_macros)]
macro_rules! map(
    { $($key:expr => $value:expr),+ } => {{
        let mut m = ::std::collections::HashMap::new();
        $(
            m.insert($key, $value);
        )+
            m
    }};
);
