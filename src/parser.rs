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
    type Token = Token;
    type Output = Output;

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

pub mod prelude {
    pub use super::terminal;
    pub use super::Error as ParserError;
    pub use super::Parser;
}
