#[derive(Debug, thiserror::Error)]
pub enum Error {}

pub trait Parser {
    type Token;
    type Output;

    fn parse(&self, input: &[Self::Token]) -> Result<(usize, Self::Output), Error>;

    fn consumes(&self) -> bool;

    fn expected(&self) -> String;
}
