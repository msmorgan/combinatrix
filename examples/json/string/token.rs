#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub enum EscapedChar {
    DoubleQuote,
    Slash,
    Backslash,
    Backspace,
    FormFeed,
    Newline,
    CarriageReturn,
    Tab,
}

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub enum Token {
    Quote,
    SimpleEscape(EscapedChar),
    UnicodeEscape(u16),
    Char(char),
}
