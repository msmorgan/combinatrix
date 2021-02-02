use std::marker::PhantomData;
use std::rc::Rc;

use either::Either;

#[derive(Debug, thiserror::Error)]
pub enum Error {
    #[error("expected {0}")]
    Expected(String),

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

type RcParser<T, O> = Rc<dyn Parser<Token = T, Output = O>>;

pub struct Terminal<Token, Output, F>(F, PhantomData<fn(&Token) -> Output>)
where
    F: Fn(&Token) -> Option<Output>;

impl<Token, Output, F> Parser for Terminal<Token, Output, F>
where
    F: Fn(&Token) -> Option<Output>,
{
    type Output = Output;
    type Token = Token;

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

pub fn terminal<Token, Output, F>(f: F) -> RcParser<Token, Output>
where
    Token: 'static,
    Output: 'static,
    F: 'static + Fn(&Token) -> Option<Output>,
{
    Rc::new(Terminal(f, PhantomData))
}

pub struct Bind<Token, OutputA, OutputB, FuncB>
where
    FuncB: Fn(OutputA) -> RcParser<Token, OutputB>,
{
    a_parser: RcParser<Token, OutputA>,
    b_func: FuncB,
}

impl<Token, OutputA, OutputB, FuncB> Parser for Bind<Token, OutputA, OutputB, FuncB>
where
    FuncB: Fn(OutputA) -> RcParser<Token, OutputB>,
{
    type Output = OutputB;
    type Token = Token;

    fn parse(&self, input: &[Self::Token]) -> Result<(usize, Self::Output), Error> {
        match self.a_parser.parse(input) {
            Ok((a_len, a_out)) => {
                let b_parser = (self.b_func)(a_out);
                match b_parser.parse(&input[a_len..]) {
                    Ok((b_len, b_out)) => Ok((a_len + b_len, b_out)),
                    Err(e) => Err(e),
                }
            }
            Err(e) => Err(e),
        }
    }

    fn consumes(&self) -> bool {
        todo!("<Bind as Parser>::consumes is hard to determine.")
    }

    fn expected(&self) -> String {
        todo!("<Bind as Parser>::expected probably justifies rewrite of Parser::expected.")
    }
}

pub fn bind<Token, OutputA, OutputB, F>(
    a_parser: RcParser<Token, OutputA>,
    b_func: F,
) -> RcParser<Token, OutputB>
where
    Token: 'static,
    OutputA: 'static,
    OutputB: 'static,
    F: 'static + Fn(OutputA) -> RcParser<Token, OutputB>,
{
    Rc::new(Bind { a_parser, b_func })
}

pub struct Seq<Token, Output>(Vec<RcParser<Token, Output>>);

impl<Token, Output> Parser for Seq<Token, Output> {
    type Output = Vec<Output>;
    type Token = Token;

    fn parse(&self, input: &[Self::Token]) -> Result<(usize, Self::Output), Error> {
        let mut outputs = vec![];
        let mut len = 0;

        for parser in &self.0 {
            match parser.parse(&input[len..]) {
                Ok((n, value)) => {
                    outputs.push(value);
                    len += n;
                }
                Err(e) => return Err(e),
            }
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

pub fn seq<Token, Output>(
    parsers: impl AsRef<[RcParser<Token, Output>]>,
) -> RcParser<Token, Vec<Output>>
where
    Token: 'static,
    Output: 'static,
{
    Rc::new(Seq(parsers.as_ref().into()))
}

pub struct Map<Token, OutputA, OutputB, F>
where
    F: Fn(OutputA) -> OutputB,
{
    a_parser: RcParser<Token, OutputA>,
    map_fn: F,
}

impl<Token, OutputA, OutputB, F> Parser for Map<Token, OutputA, OutputB, F>
where
    F: Fn(OutputA) -> OutputB,
{
    type Output = OutputB;
    type Token = Token;

    fn parse(&self, input: &[Self::Token]) -> Result<(usize, Self::Output), Error> {
        self.a_parser
            .parse(input)
            .map(|(len, value)| (len, (self.map_fn)(value)))
    }

    fn consumes(&self) -> bool {
        self.a_parser.consumes()
    }

    fn expected(&self) -> String {
        self.a_parser.expected()
    }
}

pub fn map<Token, OutputA, OutputB, F>(
    a_parser: RcParser<Token, OutputA>,
    map_fn: F,
) -> RcParser<Token, OutputB>
where
    Token: 'static,
    OutputA: 'static,
    OutputB: 'static,
    F: 'static + Fn(OutputA) -> OutputB,
{
    Rc::new(Map { a_parser, map_fn })
}

pub struct Or<Token, OutputA, OutputB>(RcParser<Token, OutputA>, RcParser<Token, OutputB>);

impl<Token, OutputA, OutputB> Parser for Or<Token, OutputA, OutputB> {
    type Output = Either<OutputA, OutputB>;
    type Token = Token;

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

pub fn or<Token, OutputA, OutputB>(
    a_parser: RcParser<Token, OutputA>,
    b_parser: RcParser<Token, OutputB>,
) -> RcParser<Token, Either<OutputA, OutputB>>
where
    Token: 'static,
    OutputA: 'static,
    OutputB: 'static,
{
    Rc::new(Or(a_parser, b_parser))
}

pub mod prelude {
    pub use super::bind;
    pub use super::map;
    pub use super::or;
    pub use super::seq;
    pub use super::terminal;
    pub use super::Error as ParserError;
    pub use super::Parser;
}
