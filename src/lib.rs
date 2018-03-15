#![allow(dead_code)]

#[macro_use]
extern crate combine;

use combine::parser::char::{letter, char, alpha_num, spaces, string};
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

#[derive(Debug, PartialEq)]
pub struct Graph {
    name: String,
    inputs: Vec<String>,
    outputs: Vec<String>,
    body: String,
}

parser! {
    fn graph[I]()(I) -> Graph
        where [I: Stream<Item=char>]
    {
        (string("graph").skip(spaces()),
         identifier().skip(spaces()),
         between(token('('), token(')'), id_list()),
         spaces().with(string("->")).skip(spaces()),
         between(token('('), token(')').skip(spaces()), id_list()),
         between(token('{'), token('}'), many(satisfy(|c| c != '}'))),
        ).map( |t| Graph{name: t.1, inputs: t.2, outputs: t.4, body: t.5})
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

    fn graph_test() {
        assert_eq!(graph().parse(r#"
graph hoge ( input ) -> (output)
{
}
"#),
                   Ok((Graph{name: "hoge".to_string(),
                             inputs: vec!["input".to_string()],
                             outputs: vec!["output".to_string()],
                             body: "".to_string()}, "")));

    }
}
