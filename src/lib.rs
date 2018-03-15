#![allow(dead_code)]

#[macro_use]
extern crate combine;

use combine::parser::char::{letter, char, alpha_num, spaces, string, newline, digit};
use combine::stream::{Stream};

use combine::*;

use std::vec::Vec;
//use std::boxed::Box;

#[derive(Debug, PartialEq)]
pub struct Ident {
    name: String
}

impl Ident {
    pub fn new<T: Into<String>>(name: T) -> Self
    {
        Ident{name: name.into()}
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
        let id_list = || sep_by(spaces().with(identifier().skip(spaces())), token(','));

        (string("graph").skip(spaces()),
         identifier().skip(spaces()),
         between(token('('), token(')'), id_list()),
         spaces().with(string("->")).skip(spaces()),
         between(token('('), token(')').skip(spaces()), id_list()),
         between(token('{'), token('}'), sep_by(spaces().with(assignment()).skip(spaces()),
                                                token(';').or(newline()))),
        ).map( |t| Graph{name: t.1, inputs: t.2, outputs: t.4, body: t.5})
    }
}

#[derive(Debug, PartialEq)]
pub enum Literal {
    Num(String),
    String(String),
    Logical(bool)
}

#[derive(Debug, PartialEq)]
pub enum LvalueExpr {
    Id(Ident),
    Array(Vec<LvalueExpr>),
    Tuple(Vec<LvalueExpr>)
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
    fn literal[I]()(I) -> Literal
        where[I: Stream<Item=char>]
    {
        // TODO: string-literal, logical-literal
        many1(digit()).map(Literal::Num)
    }
}


parser! {
    fn array_rvalue_expr[I]()(I) -> RvalueExpr
        where[I: Stream<Item=char>]
    {
        between(token('['), token(']'),
                sep_by(spaces().with(rvalue_expr().skip(spaces())), token(',')))
            .map(|v| RvalueExpr::Array(v))
    }
}

parser! {
    fn tuple_rvalue_expr[I]()(I) -> RvalueExpr
        where[I: Stream<Item=char>]
    {
        between(token('('), token(')'),
                sep_by(spaces().with(rvalue_expr().skip(spaces())), token(',')))
            .map(|v| RvalueExpr::Tuple(v))
    }
}

parser! {
    fn rvalue_expr[I]()(I) -> RvalueExpr
        where[I: Stream<Item=char>]
    {
        identifier().map(|s| RvalueExpr::Id(s))
            .or(array_rvalue_expr())
            .or(tuple_rvalue_expr())
            .or(literal().map(RvalueExpr::Lit))
    }
}

parser! {
    fn array_lvalue_expr[I]()(I) -> LvalueExpr
        where[I: Stream<Item=char>]
    {
        between(token('['), token(']'),
                sep_by(spaces().with(lvalue_expr().skip(spaces())), token(',')))
            .map(|v| LvalueExpr::Array(v))
    }
}

parser! {
    fn tuple_lvalue_expr[I]()(I) -> LvalueExpr
        where[I: Stream<Item=char>]
    {
        let with_paren = between(token('('), token(')'),
                sep_by(spaces().with(lvalue_expr().skip(spaces())), token(',')))
            .map(|v| LvalueExpr::Tuple(v));

        with_paren.or(
            (lvalue_expr(),
             many1::<Vec<LvalueExpr>, _>(spaces().with(token(',').with(spaces().with(lvalue_expr())))))
                .map(|mut t| LvalueExpr::Tuple({ t.1.insert(0, t.0); t.1})))
    }
}

parser! {
    fn lvalue_expr[I]()(I) -> LvalueExpr
        where[I: Stream<Item=char>]
    {
        identifier().map(|s| LvalueExpr::Id(s))
            //.or(literal())
            .or(array_lvalue_expr())
            .or(tuple_lvalue_expr())
    }
}


parser! {
    fn argument[I]()(I) -> Argument
        where[I: Stream<Item=char>]
    {
        let named = (identifier().skip(spaces()),
                     token('=').skip(spaces()),
                     rvalue_expr())
            .map(|t| Argument::Named(t.0, t.2));

        named.or(rvalue_expr().map(Argument::Rval))
    }
}

parser! {
    fn invocation[I]()(I) -> Invocation
        where[I: Stream<Item=char>]
    {
        (identifier().skip(spaces()),
         between(token('('), token(')'),
                 sep_by(spaces().with(argument().skip(spaces())), token(','))))
            .map(|t| Invocation{name: t.0, args: t.1})
    }
}

parser ! {
    fn assignment[I]()(I) -> Assignment
        where [I: Stream<Item=char>]
    {
        (lvalue_expr().skip(spaces()),
         token('=').skip(spaces()),
         invocation().skip(spaces()).skip(token(';')))
            .map(|t| Assignment{lexpr: t.0, invoc: t.2})
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    use combine::error::StringStreamError::*;

    #[test]
    fn id_test() {
        assert_eq!(identifier().parse("a"), Ok((Ident::new("a"), "")));
        assert_eq!(identifier().parse("abc12"), Ok((Ident::new("abc12"), "")));
        assert_eq!(identifier().parse("_12"), Ok((Ident::new("_12"), "")));

        assert_eq!(identifier().parse("123"), Err(UnexpectedParse));
        assert_eq!(identifier().parse(""), Err(UnexpectedParse));
    }

    fn graph_test() {
        assert_eq!(graph().parse(r#"
graph hoge ( input ) -> (output)
{
}
"#),
                   Ok((Graph{name: Ident::new("hoge"),
                             inputs: vec![Ident::new("input")],
                             outputs: vec![Ident::new("output")],
                             body: Vec::new()}, "")));

        assert_eq!(graph().parse(r#"
graph barfoo( input ) -> ( output )
{
    input = external(shape = [1,10])
    intermediate, extra = bar(input, alpha = 2)
    output = foo(intermediate, size = [3,5])
}
"#),
                   Ok((Graph{name: Ident::new("barfoo"),
                             inputs: vec![Ident::new("input")],
                             outputs: vec![Ident::new("output")],
                             body: vec![
                                 Assignment{lexpr: LvalueExpr::Id(Ident::new("input")),
                                            invoc: Invocation{name: Ident::new("external"),
                                                              args: vec![Argument::Named(Ident::new("shape"),
                                                                                         RvalueExpr::Array(vec![RvalueExpr::Lit(Literal::Num("1".to_string())),
                                                                                                                RvalueExpr::Lit(Literal::Num("10".to_string()))]))]}},
                                 Assignment{lexpr: LvalueExpr::Tuple(vec![LvalueExpr::Id(Ident::new("intermediate")),
                                                                          LvalueExpr::Id(Ident::new("extra"))]),
                                            invoc: Invocation{name: Ident::new("bar"),
                                                              args: vec![Argument::Rval(RvalueExpr::Id(Ident::new("input"))),
                                                                         Argument::Named(Ident::new("alpha"),
                                                                                         RvalueExpr::Lit(Literal::Num("2".to_string())))]}},
                                 Assignment{lexpr: LvalueExpr::Id(Ident::new("output")),
                                            invoc: Invocation{name: Ident::new("foo"),
                                                              args: vec![Argument::Rval(RvalueExpr::Id(Ident::new("intermediate"))),
                                                                         Argument::Named(Ident::new("size"),
                                                                                         RvalueExpr::Array(vec![RvalueExpr::Lit(Literal::Num("3".to_string())),
                                                                                                                RvalueExpr::Lit(Literal::Num("5".to_string()))]))]}},
                                 ]}, "")));
    }

    fn rvalue_expr_test() {
        assert_eq!(rvalue_expr().parse("hoge"),
                    Ok((RvalueExpr::Id(Ident::new("hoge")), "")));
    }

    fn assignment_test() {
        assert_eq!(assignment().parse("hoge = fuga( foo );"),
                   Ok((Assignment{lexpr: LvalueExpr::Id(Ident::new("hoge")),
                                  invoc: Invocation{name:Ident::new("fuga"),
                                                    args: vec![Argument::Rval(RvalueExpr::Id(Ident::new("foo")))]}}, "")));

        assert_eq!(assignment().parse("a1, a2 = fuga ( foo )"),
                   Ok((Assignment{lexpr: LvalueExpr::Tuple(vec![LvalueExpr::Id(Ident::new("a1")),
                                                                LvalueExpr::Id(Ident::new("a2"))]),
                                  invoc: Invocation{name:Ident::new("fuga"),
                                                    args: vec![Argument::Rval(RvalueExpr::Id(Ident::new("foo")))]}}, "")));
    }
}
