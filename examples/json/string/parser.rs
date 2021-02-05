use std::mem;

use combinatrix::parser::prelude::*;

use crate::string::token::EscapedChar;
use crate::string::token::Token;

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

pub fn string() -> RcParser<Token, String> {
    map(
        seq(&[
            map(quote(), |_| None),
            map(many(character()), |chars| Some(chars.into_iter().collect())),
            map(quote(), |_| None),
        ]),
        |mut results| mem::replace(results.get_mut(1).unwrap(), None).unwrap(),
    )
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
