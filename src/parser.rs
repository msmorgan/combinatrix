use std::marker::PhantomData;
use std::ops::RangeBounds;
use std::rc::Rc;

use either::Either;

#[derive(Debug, thiserror::Error)]
pub enum Error {
    #[error("expected {0}")]
    Expected(String),

    #[error("not enough repeats: {0}, encountered {1:?}")]
    Repeat(usize, Option<Box<Error>>),

    #[error("multiple errors")]
    Composite(Vec<Error>),
}

pub trait Parser {
    type Output;
    type Token;

    fn parse(&self, input: &[Self::Token]) -> Result<(usize, Self::Output), Error>;

    fn consumes(&self) -> bool;

    fn expected(&self) -> String;
}

pub type RcParser<Tok, Out> = Rc<dyn Parser<Token = Tok, Output = Out>>;

pub struct Terminal<Tok, Out, F>(F, PhantomData<fn(&Tok) -> Out>)
where
    F: Fn(&Tok) -> Option<Out>;

impl<Tok, Out, F> Parser for Terminal<Tok, Out, F>
where
    F: Fn(&Tok) -> Option<Out>,
{
    type Output = Out;
    type Token = Tok;

    fn parse(&self, input: &[Self::Token]) -> Result<(usize, Self::Output), Error> {
        input
            .iter()
            .next()
            .and_then(|token| (self.0)(token))
            .map(|value| (1, value))
            .ok_or_else(|| Error::Expected(self.expected()))
    }

    fn consumes(&self) -> bool {
        true
    }

    fn expected(&self) -> String {
        "a terminal token".to_string()
    }
}

pub fn terminal<Tok, Out, F>(f: F) -> RcParser<Tok, Out>
where
    Tok: 'static,
    Out: 'static,
    F: 'static + Fn(&Tok) -> Option<Out>,
{
    Rc::new(Terminal(f, PhantomData))
}

pub struct Bind<Tok, OutA, OutB, GetB>
where
    GetB: Fn(OutA) -> RcParser<Tok, OutB>,
{
    a_parser: RcParser<Tok, OutA>,
    b_func: GetB,
}

impl<Tok, OutA, OutB, GetB> Parser for Bind<Tok, OutA, OutB, GetB>
where
    GetB: Fn(OutA) -> RcParser<Tok, OutB>,
{
    type Output = OutB;
    type Token = Tok;

    fn parse(&self, input: &[Self::Token]) -> Result<(usize, Self::Output), Error> {
        let (a_len, a_out) = self.a_parser.parse(input)?;
        let b_parser = (self.b_func)(a_out);
        let (b_len, b_out) = b_parser.parse(&input[a_len..])?;
        Ok((a_len + b_len, b_out))
    }

    fn consumes(&self) -> bool {
        todo!("<Bind as Parser>::consumes is hard to determine.")
    }

    fn expected(&self) -> String {
        todo!("<Bind as Parser>::expected probably justifies rewrite of Parser::expected.")
    }
}

pub fn bind<Tok, OutA, OutB, GetB>(
    a_parser: RcParser<Tok, OutA>,
    b_func: GetB,
) -> RcParser<Tok, OutB>
where
    Tok: 'static,
    OutA: 'static,
    OutB: 'static,
    GetB: 'static + Fn(OutA) -> RcParser<Tok, OutB>,
{
    Rc::new(Bind { a_parser, b_func })
}

pub struct Seq<Tok, Out>(Vec<RcParser<Tok, Out>>);

impl<Tok, Out> Parser for Seq<Tok, Out> {
    type Output = Vec<Out>;
    type Token = Tok;

    fn parse(&self, input: &[Self::Token]) -> Result<(usize, Self::Output), Error> {
        let mut outputs = vec![];
        let mut len = 0;

        for parser in &self.0 {
            let (n, value) = parser.parse(&input[len..])?;
            outputs.push(value);
            len += n;
        }

        Ok((len, outputs))
    }

    fn consumes(&self) -> bool {
        self.0.iter().any(|p| p.consumes())
    }

    fn expected(&self) -> String {
        todo!("<Seq as Parser>::expected should chain its contents' expected fns.")
    }
}

pub struct Map<Tok, OutA, OutB, F>
where
    F: Fn(OutA) -> OutB,
{
    a_parser: RcParser<Tok, OutA>,
    map_fn: F,
}

impl<Tok, OutA, OutB, F> Parser for Map<Tok, OutA, OutB, F>
where
    F: Fn(OutA) -> OutB,
{
    type Output = OutB;
    type Token = Tok;

    fn parse(&self, input: &[Self::Token]) -> Result<(usize, Self::Output), Error> {
        let (len, out) = self.a_parser.parse(input)?;
        Ok((len, (self.map_fn)(out)))
    }

    fn consumes(&self) -> bool {
        self.a_parser.consumes()
    }

    fn expected(&self) -> String {
        self.a_parser.expected()
    }
}

pub fn map<Tok, OutA, OutB, F>(a_parser: RcParser<Tok, OutA>, map_fn: F) -> RcParser<Tok, OutB>
where
    Tok: 'static,
    OutA: 'static,
    OutB: 'static,
    F: 'static + Fn(OutA) -> OutB,
{
    Rc::new(Map { a_parser, map_fn })
}

pub fn seq<Tok, Out>(parsers: impl AsRef<[RcParser<Tok, Out>]>) -> RcParser<Tok, Vec<Out>>
where
    Tok: 'static,
    Out: 'static,
{
    Rc::new(Seq(parsers.as_ref().into()))
}

pub struct Alt<Tok, Out>(Vec<RcParser<Tok, Out>>);

impl<Tok, Out> Parser for Alt<Tok, Out> {
    type Output = Out;
    type Token = Tok;

    fn parse(&self, input: &[Self::Token]) -> Result<(usize, Self::Output), Error> {
        let mut errors = vec![];
        for parser in &self.0 {
            match parser.parse(input) {
                Err(e) => errors.push(e),
                ok => return ok,
            }
        }
        Err(Error::Composite(errors))
    }

    fn consumes(&self) -> bool {
        self.0.iter().all(|p| p.consumes())
    }

    fn expected(&self) -> String {
        self.0
            .iter()
            .map(|p| p.expected())
            .collect::<Vec<_>>()
            .as_slice()
            .join(" or ")
    }
}

pub fn alt<Tok, Out>(parsers: impl AsRef<[RcParser<Tok, Out>]>) -> RcParser<Tok, Out>
where
    Tok: 'static,
    Out: 'static,
{
    Rc::new(Alt(parsers.as_ref().to_vec()))
}

pub struct And<Tok, OutA, OutB> {
    a_parser: RcParser<Tok, OutA>,
    b_parser: RcParser<Tok, OutB>,
}

impl<Tok, OutA, OutB> Parser for And<Tok, OutA, OutB> {
    type Output = (OutA, OutB);
    type Token = Tok;

    fn parse(&self, input: &[Self::Token]) -> Result<(usize, Self::Output), Error> {
        let (len_a, out_a) = self.a_parser.parse(input)?;
        let (len_b, out_b) = self.b_parser.parse(&input[len_a..])?;
        Ok((len_a + len_b, (out_a, out_b)))
    }

    fn consumes(&self) -> bool {
        self.a_parser.consumes() || self.b_parser.consumes()
    }

    fn expected(&self) -> String {
        format!(
            "{} and {}",
            self.a_parser.expected(),
            self.b_parser.expected()
        )
    }
}

pub fn and<Tok, OutA, OutB>(
    a_parser: RcParser<Tok, OutA>,
    b_parser: RcParser<Tok, OutB>,
) -> RcParser<Tok, (OutA, OutB)>
where
    Tok: 'static,
    OutA: 'static,
    OutB: 'static,
{
    Rc::new(And { a_parser, b_parser })
}

pub struct Or<Tok, OutA, OutB>(RcParser<Tok, OutA>, RcParser<Tok, OutB>);

impl<Tok, OutA, OutB> Parser for Or<Tok, OutA, OutB> {
    type Output = Either<OutA, OutB>;
    type Token = Tok;

    fn parse(&self, input: &[Self::Token]) -> Result<(usize, Self::Output), Error> {
        match self.0.parse(input) {
            Ok((len, value)) => Ok((len, Either::Left(value))),
            Err(a_err) => match self.1.parse(input) {
                Ok((len, value)) => Ok((len, Either::Right(value))),
                Err(b_err) => Err(Error::Composite(vec![a_err, b_err])),
            },
        }
    }

    fn consumes(&self) -> bool {
        self.0.consumes() && self.1.consumes()
    }

    fn expected(&self) -> String {
        format!("{} or {}", self.0.expected(), self.1.expected())
    }
}

pub fn or<Tok, OutA, OutB>(
    a_parser: RcParser<Tok, OutA>,
    b_parser: RcParser<Tok, OutB>,
) -> RcParser<Tok, Either<OutA, OutB>>
where
    Tok: 'static,
    OutA: 'static,
    OutB: 'static,
{
    Rc::new(Or(a_parser, b_parser))
}

pub struct Repeat<Tok, Out> {
    parser: RcParser<Tok, Out>,
    min: usize,
    max: Option<usize>,
}

impl<Tok, Out> Parser for Repeat<Tok, Out> {
    type Output = Vec<Out>;
    type Token = Tok;

    fn parse(&self, input: &[Self::Token]) -> Result<(usize, Self::Output), Error> {
        let mut len = 0;
        let mut outputs = vec![];
        let mut last_err = None;

        dbg!(self.max);
        while dbg!(self.max.is_none() || outputs.len() < self.max.unwrap()) {
            match self.parser.parse(&input[len..]) {
                Ok((n, out)) => {
                    outputs.push(out);
                    len += n;
                }
                Err(err) => {
                    last_err = Some(Box::new(err));
                    break;
                }
            }
        }

        if outputs.len() >= self.min {
            Ok((len, outputs))
        } else {
            Err(Error::Repeat(outputs.len(), last_err))
        }
    }

    fn consumes(&self) -> bool {
        self.min > 0 && self.parser.consumes()
    }

    fn expected(&self) -> String {
        if let Some(max) = self.max {
            format!(
                "between {} and {} {}",
                self.min,
                max,
                self.parser.expected()
            )
        } else {
            format!("at least {} {}", self.min, self.parser.expected())
        }
    }
}

pub fn repeat<Tok, Out>(
    parser: RcParser<Tok, Out>,
    bounds: impl RangeBounds<usize>,
) -> RcParser<Tok, Vec<Out>>
where
    Tok: 'static,
    Out: 'static,
{
    let (min, max) = crate::get_bounds(bounds);
    Rc::new(Repeat { parser, min, max })
}

pub fn some<Tok, Out>(parser: RcParser<Tok, Out>) -> RcParser<Tok, Vec<Out>>
where
    Tok: 'static,
    Out: 'static,
{
    repeat(parser, 1..)
}

pub fn many<Tok, Out>(parser: RcParser<Tok, Out>) -> RcParser<Tok, Vec<Out>>
where
    Tok: 'static,
    Out: 'static,
{
    repeat(parser, 0..)
}

pub mod prelude {
    pub use super::alt;
    pub use super::and;
    pub use super::bind;
    pub use super::many;
    pub use super::map;
    pub use super::or;
    pub use super::repeat;
    pub use super::seq;
    pub use super::some;
    pub use super::terminal;
    pub use super::Error as ParserError;
    pub use super::Parser;
    pub use super::RcParser;
}
