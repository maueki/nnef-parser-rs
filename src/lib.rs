#![allow(dead_code)]

#[macro_use]
extern crate combine;

use combine::parser::char::{letter, char, alpha_num};
use combine::stream::{Stream};

use combine::*;

#[derive(Debug, PartialEq)]
pub enum Identifier {
    Id(String),
}

parser!{
    fn identifier[I]()(I) -> Identifier
        where [I: Stream<Item=char>]
    {
        (letter().or(char('_')),
         many(alpha_num().or(char('_'))))
            .map(|(t1, t2): (char, String)| Identifier::Id(t1.to_string() + t2.as_str()))
    }
}


#[cfg(test)]
mod tests {
    use super::*;

    use combine::error::StringStreamError::*;

    #[test]
    fn id_test() {
        assert_eq!(identifier().parse("a"), Ok((Identifier::Id("a".to_string()), "")));
        assert_eq!(identifier().parse("abc12"), Ok((Identifier::Id("abc12".to_string()), "")));
        assert_eq!(identifier().parse("_12"), Ok((Identifier::Id("_12".to_string()), "")));

        assert_eq!(identifier().parse("123"), Err(UnexpectedParse));
        assert_eq!(identifier().parse(""), Err(UnexpectedParse));
    }
}
