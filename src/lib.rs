#![feature(decl_macro)]

use std::ops::{Bound, RangeBounds};

pub mod lexer;
pub mod parser;

fn get_bounds(bounds: impl RangeBounds<usize>) -> (usize, Option<usize>) {
    let min = match bounds.start_bound() {
        Bound::Included(n) => *n,
        Bound::Excluded(n) => *n + 1,
        Bound::Unbounded => 0,
    };
    let max = match bounds.end_bound() {
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
    (min, max)
}

#[cfg(test)]
mod tests {
    #[test]
    fn it_works() {
        assert_eq!(2 + 2, 4);
    }
}
