use combinatrix::lexer::prelude::*;

use crate::string::token::EscapedChar;
use crate::string::token::Token;

fn unicode_escape() -> RcLexer {
    seq(&[
        is('\\'),
        is('u'),
        repeat(pred(|c| c.is_ascii_hexdigit()), 4..=4),
    ])
}

fn unicode_escape_value(input: &str) -> Token {
    Token::UnicodeEscape(input.bytes().skip(2).fold(0, |acc, c| {
        acc << 4
            | match c.to_ascii_lowercase() {
                b'0'..=b'9' => c - b'0',
                b'a'..=b'f' => c - b'a',
                _ => panic!("Invalid hex digit: {}", char::from(c)),
            } as u16
    }))
}

fn simple_escape() -> RcLexer {
    seq(&[is('\\'), one_of("\"\\/bfnrt")])
}

fn simple_escape_value(input: &str) -> Token {
    Token::SimpleEscape(match input.chars().nth(1).unwrap() {
        '\\' => EscapedChar::Backslash,
        '"' => EscapedChar::DoubleQuote,
        '/' => EscapedChar::Slash,
        'b' => EscapedChar::Backspace,
        'f' => EscapedChar::FormFeed,
        'n' => EscapedChar::Newline,
        'r' => EscapedChar::CarriageReturn,
        't' => EscapedChar::Tab,
        c => panic!("Invalid escape character: {}", c),
    })
}

fn legal_char() -> RcLexer {
    not(alt(&[is('"'), is('\\'), pred(|c| c.is_control())]))
}

fn legal_char_value(input: &str) -> Token {
    Token::Char(input.chars().next().unwrap())
}

fn json_string_token_map() -> TokenMap<Token> {
    token_map![
        is('"') => always!(Token::Quote),
        unicode_escape() => Box::new(unicode_escape_value),
        simple_escape() => Box::new(simple_escape_value),
        legal_char() => Box::new(legal_char_value),
    ]
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn json_string_tokens() {
        let input = "\"f\\u3938,\\\"\\\\ \"";
        let tokens = get_tokens(json_string_token_map(), input);

        assert_eq!(
            tokens,
            vec![
                Token::Quote,
                Token::Char('f'),
                Token::UnicodeEscape(0x3938),
                Token::Char(','),
                Token::SimpleEscape(EscapedChar::DoubleQuote),
                Token::SimpleEscape(EscapedChar::Backslash),
                Token::Char(' '),
                Token::Quote,
            ]
        );
    }
}
