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

fn permissive_string_lit() -> Rc<dyn Lexer> {
    let quote = is('"');

    seq(&[
        quote.clone(),
        many_until(alt(&[any(), seq(&[is('\\'), any()])]), quote.clone()),
        optional(quote.clone()),
    ])
}

fn string_lit_value(input: &str) -> Token {
    // TODO: Actually compute string value.
    Token::String(input.to_string())
}

pub fn json_token_map() -> TokenMap<Token> {
    vec![
        (pred(|c| c.is_whitespace()), always!(Token::Ignore)),
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
        (is(','), always!(Token::Punct(Punctuation::Comma))),
        (is(':'), always!(Token::Punct(Punctuation::Colon))),
        (exact("null"), always!(Token::Null)),
        (exact("false"), always!(Token::Boolean(false))),
        (exact("true"), always!(Token::Boolean(true))),
        (number_lit(), Box::new(number_lit_value)),
        (permissive_string_lit(), Box::new(string_lit_value)),
    ]
}
