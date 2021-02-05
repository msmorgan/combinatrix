use std::rc::Rc;

use combinatrix::lexer::prelude::*;

use crate::string::lexer::lex_string;
use crate::string::parser::parse_string;
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
    Token::NumberLit(
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
    let tokens = lex_string(input);
    let value = parse_string(tokens).unwrap();
    Token::StringLit(value)
}

pub fn json_token_map() -> TokenMap<Token> {
    token_map!(
        pred(|c| c.is_whitespace()) => always!(Token::Ignore),
        is('{') => always!(Token::Punct(Punctuation::Curly(Bracket::Open))),
        is('}') => always!(Token::Punct(Punctuation::Curly(Bracket::Close))),
        is('[') => always!(Token::Punct(Punctuation::Square(Bracket::Open))),
        is(']') => always!(Token::Punct(Punctuation::Square(Bracket::Close))),
        is(',') => always!(Token::Punct(Punctuation::Comma)),
        is(':') => always!(Token::Punct(Punctuation::Colon)),
        exact("null") => always!(Token::NullLit),
        exact("false") => always!(Token::BooleanLit(false)),
        exact("true") => always!(Token::BooleanLit(true)),
        number_lit() => Box::new(number_lit_value),
        permissive_string_lit() => Box::new(string_lit_value),
    )
}
