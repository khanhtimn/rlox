#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    Number(f64),
    String(String),
    Boolean(bool),
    Nil,
}

impl std::fmt::Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Nil => write!(f, "nil"),
            Value::Boolean(b) => write!(f, "{}", b),
            Value::Number(n) => {
                let s = n.to_string();
                if s.ends_with(".0") {
                    write!(f, "{}", &s[..s.len() - 2])
                } else {
                    write!(f, "{}", s)
                }
            }
            Value::String(s) => write!(f, "{}", s),
        }
    }
}
