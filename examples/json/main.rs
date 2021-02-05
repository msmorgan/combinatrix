mod lexer;
mod parser;
mod token;
mod string {
    pub mod lexer;
    pub mod parser;
    pub mod token;
}

pub enum Value {
    String(String),
    Number(f64),
    Boolean(bool),
    Null,
    Array(Vec<Value>),
    Object(Vec<(String, Value)>),
}

fn main() {}
