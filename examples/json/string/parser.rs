use std::mem;

use combinatrix::parser::prelude::*;

use crate::string::token::{EscapedChar, Token};

fn quote() -> RcParser<Token, ()> {
    terminal(|t| {
        if matches!(t, Token::Quote) {
            Some(())
        } else {
            None
        }
    })
}

fn character() -> RcParser<Token, char> {
    terminal(|t| match t {
        Token::Char(c) => Some(*c),
        Token::SimpleEscape(escaped) => Some(match escaped {
            EscapedChar::DoubleQuote => '"',
            EscapedChar::Slash => '/',
            EscapedChar::Backslash => '\\',
            EscapedChar::Backspace => '\x08',
            EscapedChar::FormFeed => '\x0c',
            EscapedChar::Newline => '\n',
            EscapedChar::CarriageReturn => '\r',
            EscapedChar::Tab => '\t',
        }),
        Token::UnicodeEscape(code) => std::char::from_u32(*code as u32),
        _ => None,
    })
}

fn string() -> RcParser<Token, String> {
    map(
        seq(&[
            map(quote(), |_| None),
            map(many(character()), |chars| Some(chars.into_iter().collect())),
            map(quote(), |_| None),
        ]),
        |mut results: Vec<Option<String>>| results[1].take().unwrap(),
    )
}

pub fn parse_string(tokens: impl AsRef<[Token]>) -> Result<String, ParserError> {
    string().parse(tokens.as_ref()).map(|(_, out)| out)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parse_string() {
        let output = string().parse(&[
            Token::Quote,
            Token::Char('h'),
            Token::Char('e'),
            Token::Char('l'),
            Token::Char('l'),
            Token::Char('o'),
            Token::Char('!'),
            Token::SimpleEscape(EscapedChar::Newline),
            Token::Quote,
        ]);

        assert_eq!(output.unwrap(), (9, "hello!\n".to_string()));
    }
}
