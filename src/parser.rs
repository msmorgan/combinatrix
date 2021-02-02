use std::marker::PhantomData;
use std::rc::Rc;

#[derive(Debug, thiserror::Error)]
pub enum Error {
    #[error("expected {0}")]
    Expected(String),
}

pub trait Parser {
    type Token;
    type Output;

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

pub mod prelude {
    pub use super::bind;
    pub use super::terminal;
    pub use super::Error as ParserError;
    pub use super::Parser;
}
