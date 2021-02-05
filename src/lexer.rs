use std::ops::RangeBounds;
use std::rc::Rc;

#[derive(Debug, thiserror::Error)]
pub enum Error {
    #[error("expected {0}")]
    Expected(String),
}

pub trait Lexer {
    fn lex(&self, input: &str) -> Option<usize>;
    fn consumes(&self) -> bool;
    fn expected(&self) -> String;

    fn lex_result(&self, input: &str) -> Result<usize, Error> {
        self.lex(input)
            .ok_or_else(|| Error::Expected(self.expected()))
    }
}

pub type RcLexer = Rc<dyn Lexer>;

pub struct Pred<F>(F)
where
    F: Fn(char) -> bool;

impl<F> Lexer for Pred<F>
where
    F: Fn(char) -> bool,
{
    fn lex(&self, input: &str) -> Option<usize> {
        input.chars().next().and_then(|c| {
            if (self.0)(c) {
                Some(c.len_utf8())
            } else {
                None
            }
        })
    }

    fn consumes(&self) -> bool {
        true
    }

    fn expected(&self) -> String {
        "character matching predicate".to_string()
    }
}

pub fn pred(predicate: impl 'static + Fn(char) -> bool) -> RcLexer {
    Rc::new(Pred(predicate))
}

pub struct Is(char);

impl Lexer for Is {
    fn lex(&self, input: &str) -> Option<usize> {
        input.chars().next().and_then(|c| {
            if c == self.0 {
                Some(c.len_utf8())
            } else {
                None
            }
        })
    }

    fn consumes(&self) -> bool {
        true
    }

    fn expected(&self) -> String {
        format!("'{}'", self.0)
    }
}

pub fn is(c: char) -> RcLexer {
    Rc::new(Is(c))
}

pub struct Exact(String);

impl Lexer for Exact {
    fn lex(&self, input: &str) -> Option<usize> {
        if input.starts_with(&self.0) {
            Some(self.0.len())
        } else {
            None
        }
    }

    fn consumes(&self) -> bool {
        self.0.len() > 0
    }

    fn expected(&self) -> String {
        format!("the string \"{}\"", &self.0)
    }
}

pub fn exact(string: impl AsRef<str>) -> RcLexer {
    Rc::new(Exact(string.as_ref().to_string()))
}

fn join_with_last<S: ToString>(items: impl AsRef<[S]>, sep: &str, last_sep: &str) -> String {
    let items = items.as_ref();

    let mut buf = String::new();

    let mut iter = items.iter();
    let mut first = true;
    let mut next = iter.next();
    while let Some(item) = next {
        let item = item.to_string();
        next = iter.next();

        let last = next.is_none();
        buf.push_str(&format!(
            "{}{}",
            if first {
                ""
            } else {
                if last { last_sep } else { sep }
            },
            item
        ));
        first = false;
    }

    buf
}

pub struct OneOf(String);

impl Lexer for OneOf {
    fn lex(&self, input: &str) -> Option<usize> {
        input.chars().next().and_then(|c| {
            if self.0.contains(c) {
                Some(c.len_utf8())
            } else {
                None
            }
        })
    }

    fn consumes(&self) -> bool {
        true
    }

    fn expected(&self) -> String {
        format!(
            "one of {}",
            join_with_last(
                self.0
                    .chars()
                    .map(|c| format!("'{}'", c))
                    .collect::<Vec<_>>(),
                ", ",
                ", or ",
            )
        )
    }
}

pub fn one_of(chars: impl AsRef<str>) -> RcLexer {
    Rc::new(OneOf(chars.as_ref().to_string()))
}

pub struct Any;

impl Lexer for Any {
    fn lex(&self, input: &str) -> Option<usize> {
        input.chars().next().map(|_| 1)
    }

    fn consumes(&self) -> bool {
        true
    }

    fn expected(&self) -> String {
        "any character".to_string()
    }
}

pub fn any() -> RcLexer {
    Rc::new(Any)
}

pub struct CharRange(char, char);

impl Lexer for CharRange {
    fn lex(&self, input: &str) -> Option<usize> {
        input.chars().next().and_then(|c| {
            if c >= self.0 && c <= self.1 {
                Some(c.len_utf8())
            } else {
                None
            }
        })
    }

    fn consumes(&self) -> bool {
        true
    }

    fn expected(&self) -> String {
        format!("a character between '{}' and '{}'", self.0, self.1)
    }
}

pub fn char_range(start: char, end: char) -> RcLexer {
    Rc::new(CharRange(start, end))
}

pub struct Repeat {
    lexer: RcLexer,
    min: usize,
    max: Option<usize>,
}

impl Lexer for Repeat {
    fn lex(&self, input: &str) -> Option<usize> {
        let mut count = 0;
        let mut len = 0;
        while count < self.max.unwrap_or(usize::MAX) {
            if let Some(n) = self.lexer.lex(&input[len..]) {
                count += 1;
                len += n;
            } else {
                break;
            }
        }
        if count >= self.min { Some(len) } else { None }
    }

    fn consumes(&self) -> bool {
        self.min >= 1 && self.lexer.consumes()
    }

    fn expected(&self) -> String {
        let quantity = match self.max {
            Some(max) => format!("between {} and {}", self.min, max),

            None => format!("at least {}", self.min),
        };
        format!("{} {}", quantity, self.lexer.expected())
    }
}

pub fn repeat(lexer: RcLexer, bounds: impl RangeBounds<usize>) -> RcLexer {
    let (min, max) = crate::get_bounds(bounds);
    Rc::new(Repeat { lexer, min, max })
}

pub fn optional(lexer: RcLexer) -> RcLexer {
    repeat(lexer, 0..=1)
}

pub fn some(lexer: RcLexer) -> RcLexer {
    repeat(lexer, 1..)
}

pub fn many(lexer: RcLexer) -> RcLexer {
    repeat(lexer, ..)
}

pub struct Alt(Vec<RcLexer>);

impl Lexer for Alt {
    fn lex(&self, input: &str) -> Option<usize> {
        for lexer in &self.0 {
            if let Some(len) = lexer.lex(input) {
                return Some(len);
            }
        }
        None
    }

    fn consumes(&self) -> bool {
        self.0.iter().all(|l| l.consumes())
    }

    fn expected(&self) -> String {
        join_with_last(
            self.0.iter().map(|l| l.expected()).collect::<Vec<_>>(),
            ", ",
            ", or ",
        )
    }
}

pub fn alt(lexers: impl AsRef<[RcLexer]>) -> RcLexer {
    Rc::new(Alt(lexers.as_ref().into()))
}

pub struct Seq(Vec<RcLexer>);

impl Lexer for Seq {
    fn lex(&self, input: &str) -> Option<usize> {
        let mut len = 0;
        for lexer in &self.0 {
            if let Some(n) = lexer.lex(&input[len..]) {
                len += n;
            } else {
                return None;
            }
        }
        Some(len)
    }

    fn consumes(&self) -> bool {
        self.0.iter().any(|l| l.consumes())
    }

    fn expected(&self) -> String {
        todo!("<Seq as Lexer>::expected seems complicated since I'm not sure where the error was.")
    }
}

pub fn seq(lexers: impl AsRef<[RcLexer]>) -> RcLexer {
    Rc::new(Seq(lexers.as_ref().into()))
}

struct Reject(RcLexer);

impl Lexer for Reject {
    fn lex(&self, input: &str) -> Option<usize> {
        if self.0.lex(input).is_none() {
            Some(0)
        } else {
            None
        }
    }

    fn consumes(&self) -> bool {
        false
    }

    fn expected(&self) -> String {
        todo!("<Reject as Lexer>::expected is confusing")
    }
}

pub fn reject(lexer: RcLexer) -> RcLexer {
    Rc::new(Reject(lexer))
}

pub fn not(lexer: RcLexer) -> RcLexer {
    seq(&[reject(lexer), any()])
}

pub fn many_until(lexer: RcLexer, stop_before: RcLexer) -> RcLexer {
    many(seq(&[reject(stop_before), lexer]))
}

pub type TokenMap<T> = Vec<(RcLexer, Box<dyn Fn(&str) -> T>)>;

pub macro token_map($($lexer:expr => $to_token:expr),* $(,)?) {
    vec![$(
        ($lexer, $to_token),
    )*]
}

pub macro always($value:expr) {
    Box::new(|_| $value)
}

pub fn get_tokens<T>(token_map: TokenMap<T>, input: &str) -> Vec<T> {
    let mut tokens = vec![];
    let mut pos = 0;
    'outer: while pos < input.len() {
        for (lexer, make_token) in &token_map {
            if let Some(n) = lexer.lex(&input[pos..]) {
                let token = make_token(&input[pos..pos + n]);
                tokens.push(token);
                pos += n;
                continue 'outer;
            }
        }
        break;
    }
    tokens
}

pub mod prelude {
    pub use super::{
        alt,
        always,
        any,
        char_range,
        exact,
        get_tokens,
        is,
        many,
        many_until,
        not,
        one_of,
        optional,
        pred,
        reject,
        repeat,
        seq,
        some,
        token_map,
        Lexer,
        RcLexer,
        TokenMap,
    };
}
