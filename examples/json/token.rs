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
    Null,
    Boolean(bool),
    Number(f64),
    String(String),
    Unexpected(char),
}
