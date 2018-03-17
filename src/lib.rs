#![allow(dead_code)]

#[macro_use]
extern crate combine;

use combine::parser::char::{letter, char, alpha_num, space, string, digit};
use combine::stream::Stream;

use combine::*;

use std::vec::Vec;
//use std::boxed::Box;

#[derive(Debug, PartialEq)]
pub struct Ident {
    name: String,
}

impl Ident {
    pub fn new<T: Into<String>>(name: T) -> Self {
        Ident { name: name.into() }
    }
}

parser! {
    fn whitespace[I]()(I) -> ()
        where [I: Stream<Item=char>]
    {
        let comment = (token('#'), skip_many(satisfy(|c| c != '\n'))).map(|_| ());
        skip_many(skip_many1(space()).or(comment))
    }
}

parser! {
    fn identifier[I]()(I) -> Ident
        where [I: Stream<Item=char>]
    {
        (letter().or(char('_')),
         many(alpha_num().or(char('_'))))
            .map(|(t1, t2): (char, String)| Ident::new(t1.to_string() + t2.as_str()))
    }
}

#[derive(Debug, PartialEq)]
pub struct Graph {
    name: Ident,
    inputs: Vec<Ident>,
    outputs: Vec<Ident>,
    body: Vec<Assignment>,
}

parser! {
    fn graph[I]()(I) -> Graph
        where [I: Stream<Item=char>]
    {
        let id_list = || sep_by(whitespace().with(identifier().skip(whitespace())), token(','));

        (string("graph").skip(whitespace()),
         identifier().skip(whitespace()),
         between(token('('), token(')'), id_list()),
         whitespace().with(string("->")).skip(whitespace()),
         between(token('('), token(')').skip(whitespace()), id_list()),
         between(token('{').skip(whitespace()), whitespace().with(token('}')),
                 many((whitespace(), assignment()).map(|t| t.1)))
        ).map( |t| Graph{name: t.1, inputs: t.2, outputs: t.4, body: t.5})
    }
}

#[derive(Debug, PartialEq)]
pub struct Numeric {
    int: String,
    frac: Option<String>,
    exp: Option<String>,
}

impl Numeric {
    #[cfg(test)]
    pub fn new<T: Into<String>>(i: T) -> Self {
        Numeric {
            int: i.into(),
            frac: None,
            exp: None,
        }
    }

    #[cfg(test)]
    pub fn new_fe<T: Into<String>>(i: T, f: Option<T>, e: Option<T>) -> Self {
        Numeric {
            int: i.into(),
            frac: f.map(|c| c.into()),
            exp: e.map(|c| c.into()),
        }
    }
}

#[derive(Debug, PartialEq)]
pub enum Literal {
    Num(Numeric),
    String(String),
    Logical(bool),
}

#[derive(Debug, PartialEq)]
pub enum LvalueExpr {
    Id(Ident),
    Array(Vec<LvalueExpr>),
    Tuple(Vec<LvalueExpr>),
}

#[derive(Debug, PartialEq)]
pub enum RvalueExpr {
    Id(Ident),
    Lit(Literal),
    Array(Vec<RvalueExpr>),
    Tuple(Vec<RvalueExpr>),
}

#[derive(Debug, PartialEq)]
pub enum Argument {
    Rval(RvalueExpr),
    Named(Ident, RvalueExpr),
}

#[derive(Debug, PartialEq)]
pub struct Invocation {
    name: Ident,
    args: Vec<Argument>,
}

#[derive(Debug, PartialEq)]
pub struct Assignment {
    lexpr: LvalueExpr,
    invoc: Invocation,
}

parser! {
    fn numeric[I]()(I) -> Numeric
        where[I: Stream<Item=char>]
    {
        let unsigned = || many1(digit());
        let signed = || token('-').with(unsigned()).map(|x: String| "-".to_string() + x.as_ref());

        (signed().or(unsigned()),
         optional(token('.').with(many1(digit()))),
         optional(token('e').or(token('E')).with(signed().or(unsigned()))))
            .map(|t| Numeric{int: t.0, frac: t.1, exp: t.2})
    }
}

parser! {
    fn literal[I]()(I) -> Literal
        where[I: Stream<Item=char>]
    {
        // TODO: float-literal, escape sequence in string-literal
        choice! {
            numeric().map(Literal::Num),
            between(token('"'), token('"'),
                    many(satisfy(|c| c != '"'))).map(Literal::String),
            try(string("true")).map(|_| Literal::Logical(true)),
            try(string("false")).map(|_| Literal::Logical(false))
        }
    }
}


parser! {
    fn array_rvalue_expr[I]()(I) -> RvalueExpr
        where[I: Stream<Item=char>]
    {
        between(token('['), token(']'),
                sep_by(whitespace().with(rvalue_expr().skip(whitespace())), token(',')))
            .map(|v| RvalueExpr::Array(v))
    }
}

parser! {
    fn tuple_rvalue_expr[I]()(I) -> RvalueExpr
        where[I: Stream<Item=char>]
    {
        between(token('('), token(')'),
                sep_by(whitespace().with(rvalue_expr().skip(whitespace())), token(',')))
            .map(|v| RvalueExpr::Tuple(v))
    }
}

parser! {
    fn rvalue_expr[I]()(I) -> RvalueExpr
        where[I: Stream<Item=char>]
    {
        choice! {
            literal().map(RvalueExpr::Lit),
            identifier().map(RvalueExpr::Id),
            array_rvalue_expr(),
            tuple_rvalue_expr()
        }
    }
}

parser! {
    fn array_lvalue_expr[I]()(I) -> LvalueExpr
        where[I: Stream<Item=char>]
    {
        between(token('['), token(']'),
                sep_by(whitespace().with(lvalue_expr().skip(whitespace())), token(',')))
            .map(|v| LvalueExpr::Array(v))
    }
}

parser! {
    fn tuple_lvalue_expr[I]()(I) -> LvalueExpr
        where[I: Stream<Item=char>]
    {
        let with_paren = between(token('('), token(')'),
                sep_by(whitespace().with(lvalue_expr().skip(whitespace())), token(',')))
            .map(|v| LvalueExpr::Tuple(v));

        // TODO: why match failed?
        // let without_paren = (identifier().map(LvalueExpr::Id),
        //                      many1(whitespace().with(token(',')).skip(whitespace())
        //                            .with(identifier().map(LvalueExpr::Id))))

        let without_paren = (identifier().map(LvalueExpr::Id).skip(whitespace()),
                             many1(token(',').skip(whitespace())
                                   .with(identifier().map(LvalueExpr::Id)).skip(whitespace())))
            .map(|(i, mut is): (LvalueExpr, Vec<LvalueExpr>)| LvalueExpr::Tuple({
                is.insert(0, i);
                is
            }));

        choice! {
            with_paren,
            try(without_paren)
        }
    }
}

parser! {
    fn lvalue_expr[I]()(I) -> LvalueExpr
        where[I: Stream<Item=char>]
    {
        choice! {
            array_lvalue_expr(),
            try(tuple_lvalue_expr()), // why need try??
            identifier().map(LvalueExpr::Id)
        }
    }
}


parser! {
    fn argument[I]()(I) -> Argument
        where[I: Stream<Item=char>]
    {
        let named = (identifier().skip(whitespace()),
                     token('=').skip(whitespace()),
                     rvalue_expr())
            .map(|t| Argument::Named(t.0, t.2));

        choice! {
            try(named),
            rvalue_expr().map(Argument::Rval)
        }
    }
}

parser! {
    fn invocation[I]()(I) -> Invocation
        where[I: Stream<Item=char>]
    {
        (identifier().skip(whitespace()),
         between(token('('), token(')'),
                 sep_by(whitespace().with(argument()).skip(whitespace()), token(','))))
            .map(|t| Invocation{name: t.0, args: t.1})
    }
}

parser ! {
    fn assignment[I]()(I) -> Assignment
        where [I: Stream<Item=char>]
    {
        (lvalue_expr(),
         whitespace().with(token('=')).skip(whitespace()),
         invocation().skip(whitespace().with(optional(token(';')))))
            .map(|t| Assignment{lexpr: t.0, invoc: t.2})
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    use combine::error::StringStreamError::*;

    #[test]
    fn identifier_test() {
        assert_eq!(identifier().parse("a"), Ok((Ident::new("a"), "")));
        assert_eq!(identifier().parse("a2"), Ok((Ident::new("a2"), "")));
        assert_eq!(identifier().parse("abc12"), Ok((Ident::new("abc12"), "")));
        assert_eq!(identifier().parse("_12"), Ok((Ident::new("_12"), "")));
        assert_eq!(identifier().parse("foo"), Ok((Ident::new("foo"), "")));

        assert_eq!(identifier().parse("123"), Err(UnexpectedParse));
        assert_eq!(identifier().parse(""), Err(UnexpectedParse));
    }

    #[test]
    fn graph_test() {
        assert_eq!(
            graph().parse(
                r#"graph hoge ( input ) -> (output)
{
}"#,
            ),
            Ok((
                Graph {
                    name: Ident::new("hoge"),
                    inputs: vec![Ident::new("input")],
                    outputs: vec![Ident::new("output")],
                    body: Vec::new(),
                },
                "",
            ))
        );

        assert_eq!(
            graph().easy_parse(
                r#"graph barfoo( input ) -> ( output )
{ # comment
    # comment
    input = external(shape = [1,10]) #comment
    intermediate, extra = bar(input, alpha = 2)
    output = foo(intermediate, size = [3,5])
}"#,
            ),
            Ok((
                Graph {
                    name: Ident::new("barfoo"),
                    inputs: vec![Ident::new("input")],
                    outputs: vec![Ident::new("output")],
                    body: vec![
                        Assignment {
                            lexpr: LvalueExpr::Id(Ident::new("input")),
                            invoc: Invocation {
                                name: Ident::new("external"),
                                args: vec![
                                    Argument::Named(
                                        Ident::new("shape"),
                                        RvalueExpr::Array(vec![
                                            RvalueExpr::Lit(Literal::Num(Numeric::new("1"))),
                                            RvalueExpr::Lit(Literal::Num(Numeric::new("10"))),
                                        ])
                                    ),
                                ],
                            },
                        },
                        Assignment {
                            lexpr: LvalueExpr::Tuple(vec![
                                LvalueExpr::Id(Ident::new("intermediate")),
                                LvalueExpr::Id(Ident::new("extra")),
                            ]),
                            invoc: Invocation {
                                name: Ident::new("bar"),
                                args: vec![
                                    Argument::Rval(RvalueExpr::Id(Ident::new("input"))),
                                    Argument::Named(
                                        Ident::new("alpha"),
                                        RvalueExpr::Lit(Literal::Num(Numeric::new("2")))
                                    ),
                                ],
                            },
                        },
                        Assignment {
                            lexpr: LvalueExpr::Id(Ident::new("output")),
                            invoc: Invocation {
                                name: Ident::new("foo"),
                                args: vec![
                                    Argument::Rval(
                                        RvalueExpr::Id(Ident::new("intermediate"))
                                    ),
                                    Argument::Named(
                                        Ident::new("size"),
                                        RvalueExpr::Array(vec![
                                            RvalueExpr::Lit(Literal::Num(Numeric::new("3"))),
                                            RvalueExpr::Lit(Literal::Num(Numeric::new("5"))),
                                        ])
                                    ),
                                ],
                            },
                        },
                    ],
                },
                "",
            ))
        );
    }

    #[test]
    fn lvalue_expr_test() {
        assert_eq!(
            lvalue_expr().parse("hoge"),
            Ok((LvalueExpr::Id(Ident::new("hoge")), "")));

        assert_eq!(
            lvalue_expr().parse("hoge, fuga"),
            Ok((LvalueExpr::Tuple(vec![LvalueExpr::Id(Ident::new("hoge")),
                                       LvalueExpr::Id(Ident::new("fuga"))]), "")));

        assert_eq!(
            lvalue_expr().parse("a1, a2 ="),
            Ok((LvalueExpr::Tuple(vec![LvalueExpr::Id(Ident::new("a1")),
                                       LvalueExpr::Id(Ident::new("a2"))]), "=")));
    }

    #[test]
    fn rvalue_expr_test() {
        assert_eq!(
            rvalue_expr().parse("hoge"),
            Ok((RvalueExpr::Id(Ident::new("hoge")), ""))
        );

        assert_eq!(
            rvalue_expr().parse("foo"),
            Ok((RvalueExpr::Id(Ident::new("foo")), ""))
        );
    }

    #[test]
    fn argument_test() {
        assert_eq!(
            argument().parse("foo"),
            Ok((Argument::Rval(RvalueExpr::Id(Ident::new("foo"))), ""))
        );
    }

    #[test]
    fn invocation_test() {
        assert_eq!(
            invocation().parse("fuga( foo )"),
            Ok((
                Invocation {
                    name: Ident::new("fuga"),
                    args: vec![Argument::Rval(RvalueExpr::Id(Ident::new("foo")))],
                },
                "",
            ))
        );
    }

    #[test]
    fn assignment_test() {
        assert_eq!(
            assignment().parse("hoge = fuga( foo ) ;"),
            Ok((
                Assignment {
                    lexpr: LvalueExpr::Id(Ident::new("hoge")),
                    invoc: Invocation {
                        name: Ident::new("fuga"),
                        args: vec![Argument::Rval(RvalueExpr::Id(Ident::new("foo")))],
                    },
                },
                "",
            ))
        );

        assert_eq!(assignment().parse("a1, a2 = fuga ( foo );"),
                   Ok((Assignment{lexpr: LvalueExpr::Tuple(vec![LvalueExpr::Id(Ident::new("a1")),
                                                                LvalueExpr::Id(Ident::new("a2"))]),
                                  invoc: Invocation{name:Ident::new("fuga"),
                                                    args: vec![Argument::Rval(RvalueExpr::Id(Ident::new("foo")))]}}, "")));
    }

    #[test]
    fn literal_test() {
        assert_eq!(
            literal().parse("123"),
            Ok((Literal::Num(Numeric::new("123")), ""))
        );
        assert_eq!(
            literal().parse(r#""test""#),
            Ok((Literal::String("test".to_string()), ""))
        );
        assert_eq!(literal().parse("true"), Ok((Literal::Logical(true), "")));
        assert_eq!(literal().parse("false"), Ok((Literal::Logical(false), "")));
    }

    #[test]
    fn numeric_test() {
        assert_eq!(
            numeric().parse("123e3"),
            Ok((Numeric::new_fe("123", None, Some("3")), ""))
        );
        assert_eq!(
            numeric().parse("-123.2e3"),
            Ok((Numeric::new_fe("-123", Some("2"), Some("3")), ""))
        );
    }

}
