use std::rc::Rc;

use combinatrix::lexer::prelude::*;

use crate::token::*;

fn number_lit() -> Rc<dyn Lexer> {
    let digits = many(pred(|c| matches!(c, '0'..='9')));

    let sign = is('-');
    let whole = alt(&[
        is('0'),
        seq(&[pred(|c| matches!(c, '1'..='9')), digits.clone()]),
    ]);
    let frac = seq(&[is('.'), digits.clone()]);
    let exp = seq(&[one_of("eE"), optional(one_of("+-")), digits.clone()]);

    seq(&[optional(sign), whole, optional(frac), optional(exp)])
}

fn number_lit_value(input: &str) -> Token {
    Token::Number(
        input
            .parse()
            .expect("invalid number literal lexed as number literal"),
    )
}

pub fn json_token_map() -> TokenMap<Token> {
    vec![
        (pred(|c| c.is_whitespace()), Box::new(|_| Token::Ignore)),
        (
            is('{'),
            always!(Token::Punct(Punctuation::Curly(Bracket::Open))),
        ),
        (
            is('}'),
            always!(Token::Punct(Punctuation::Curly(Bracket::Close))),
        ),
        (
            is('['),
            always!(Token::Punct(Punctuation::Square(Bracket::Open))),
        ),
        (
            is(']'),
            always!(Token::Punct(Punctuation::Square(Bracket::Close))),
        ),
        (is(','), Box::new(|_| Token::Punct(Punctuation::Comma))),
        (is(':'), Box::new(|_| Token::Punct(Punctuation::Colon))),
        (exact("null"), Box::new(|_| Token::Null)),
        (exact("false"), Box::new(|_| Token::Boolean(false))),
        (exact("true"), Box::new(|_| Token::Boolean(true))),
        (number_lit(), Box::new(number_lit_value)),
    ]
}