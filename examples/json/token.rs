#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub enum Bracket {
    Open,
    Close,
}

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub enum Punctuation {
    Comma,
    Colon,
    Square(Bracket),
    Curly(Bracket),
}

#[derive(Debug, Clone)]
pub enum Token {
    Ignore,
    Punct(Punctuation),
    NullLit,
    BooleanLit(bool),
    NumberLit(f64),
    StringLit(String),
    Unexpected(char),
}
