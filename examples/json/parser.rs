use combinatrix::parser::prelude::*;

use super::token::{Bracket, Punctuation, Token};
use super::Value;

fn string() -> RcParser<Token, Value> {
    terminal(|t| match t {
        Token::StringLit(s) => Some(Value::String(s.clone())),
        _ => None,
    })
}

fn number() -> RcParser<Token, Value> {
    terminal(|t| {
        if let Token::NumberLit(value) = t {
            Some(Value::Number(*value))
        } else {
            None
        }
    })
}

fn boolean() -> RcParser<Token, Value> {
    terminal(|t| {
        if let Token::BooleanLit(value) = t {
            Some(Value::Boolean(*value))
        } else {
            None
        }
    })
}

fn null() -> RcParser<Token, Value> {
    terminal(|t| {
        if let Token::NullLit = t {
            Some(Value::Null)
        } else {
            None
        }
    })
}

fn array() -> RcParser<Token, Value> {
    let open_brace: RcParser<Token, Option<Vec<Value>>> = terminal(|t| {
        if matches!(t, Token::Punct(Punctuation::Square(Bracket::Open))) {
            Some(None)
        } else {
            None
        }
    });
    let close_brace: RcParser<Token, Option<Vec<Value>>> = terminal(|t| {
        if matches!(t, Token::Punct(Punctuation::Square(Bracket::Close))) {
            Some(None)
        } else {
            None
        }
    });

    unimplemented!()
}
