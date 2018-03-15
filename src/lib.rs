#![allow(dead_code)]

#[macro_use]
extern crate combine;

use combine::parser::char::{letter, char, alpha_num, spaces};
use combine::stream::{Stream};

use combine::*;

parser! {
    fn identifier[I]()(I) -> String
        where [I: Stream<Item=char>]
    {
        (letter().or(char('_')),
         many(alpha_num().or(char('_'))))
            .map(|(t1, t2): (char, String)| t1.to_string() + t2.as_str())
    }
}

parser! {
    fn id_list[I]()(I) -> Vec<String>
        where [I: Stream<Item=char>]
    {
        sep_by(spaces().with(identifier().skip(spaces())), token(','))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    use combine::error::StringStreamError::*;

    #[test]
    fn id_test() {
        assert_eq!(identifier().parse("a"), Ok(("a".to_string(), "")));
        assert_eq!(identifier().parse("abc12"), Ok(("abc12".to_string(), "")));
        assert_eq!(identifier().parse("_12"), Ok(("_12".to_string(), "")));

        assert_eq!(identifier().parse("123"), Err(UnexpectedParse));
        assert_eq!(identifier().parse(""), Err(UnexpectedParse));
    }

    fn id_list_test() {
        assert_eq!(id_list().parse("a"), Ok((vec!["a".to_string()], "")));
        assert_eq!(id_list().parse(" a, b "), Ok((vec!["a".to_string(), "b".to_string()], "")));
    }
}
