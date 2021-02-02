use std::collections::Bound;
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

pub fn pred(predicate: impl 'static + Fn(char) -> bool) -> Rc<dyn Lexer> {
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

pub fn is(c: char) -> Rc<dyn Lexer> {
    Rc::new(Is(c))
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

pub fn one_of(chars: impl AsRef<str>) -> Rc<dyn Lexer> {
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

pub fn any() -> Rc<dyn Lexer> {
    Rc::new(Any)
}

pub struct Repeat {
    lexer: Rc<dyn Lexer>,
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

fn repeat(lexer: Rc<dyn Lexer>, bounds: impl RangeBounds<usize>) -> Rc<dyn Lexer> {
    let min = match bounds.start_bound() {
        Bound::Included(n) => *n,
        Bound::Excluded(n) => *n + 1,
        Bound::Unbounded => 0,
    };
    let max = match bounds.start_bound() {
        Bound::Included(n) => {
            if *n < usize::MAX {
                Some(*n + 1)
            } else {
                None
            }
        }
        Bound::Excluded(n) => Some(*n),
        Bound::Unbounded => None,
    };
    Rc::new(Repeat { lexer, min, max })
}

pub fn optional(lexer: Rc<dyn Lexer>) -> Rc<dyn Lexer> {
    repeat(lexer, 0..=1)
}

pub fn some(lexer: Rc<dyn Lexer>) -> Rc<dyn Lexer> {
    repeat(lexer, 1..)
}

pub fn many(lexer: Rc<dyn Lexer>) -> Rc<dyn Lexer> {
    repeat(lexer, ..)
}

pub struct Alt(Vec<Rc<dyn Lexer>>);

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

pub fn alt(lexers: impl AsRef<[Rc<dyn Lexer>]>) -> Rc<dyn Lexer> {
    Rc::new(Alt(lexers.as_ref().into()))
}

pub struct Seq(Vec<Rc<dyn Lexer>>);

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

pub fn seq(lexers: impl AsRef<[Rc<dyn Lexer>]>) -> Rc<dyn Lexer> {
    Rc::new(Seq(lexers.as_ref().into()))
}

pub type TokenMap<T> = Vec<(Rc<dyn Lexer>, Box<dyn Fn(&str) -> T>)>;

pub macro always($value:expr) {
    Box::new(|_| $value)
}

pub mod prelude {
    pub use super::alt;
    pub use super::always;
    pub use super::any;
    pub use super::is;
    pub use super::many;
    pub use super::one_of;
    pub use super::optional;
    pub use super::pred;
    pub use super::seq;
    pub use super::some;
    pub use super::Lexer;
    pub use super::TokenMap;
}
