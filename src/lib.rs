#![allow(dead_code)]

#[macro_use]
extern crate combine;

use combine::parser::char::{letter, char, alpha_num, space, spaces, string, digit};
use combine::stream::Stream;
use combine::parser::sequence::Skip;
use combine::easy;
use combine::*;

use std::vec::Vec;

pub type Errors<I> = easy::Errors<char, <I as StreamOnce>::Range, <I as StreamOnce>::Position>;

pub fn parse_doc<I>(
    input: I,
) -> Result<Document, Errors<I>>
where
    I: Stream<Item = char>,
    <I as StreamOnce>::Error: ParseError<char, <I as StreamOnce>::Range, <I as StreamOnce>::Position>,
    <I as StreamOnce>::Position: std::default::Default,
{
    document().easy_parse(input).map(|(d, _)| d)
}

#[derive(Debug, PartialEq)]
pub struct Ident {
    pub name: String,
}

fn lex<P>(p: P) -> Skip<P, whitespace<P::Input>>
where
    P: Parser,
    P::Input: Stream<Item = char>,
    <P::Input as StreamOnce>::Error: ParseError<
        <P::Input as StreamOnce>::Item,
        <P::Input as StreamOnce>::Range,
        <P::Input as StreamOnce>::Position,
    >,
{
    p.skip(whitespace())
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

parser! {
    fn version[I]()(I) -> Numeric
        where [I: Stream<Item=char>]
    {
        try(string("version").with(spaces()).with(numeric()))
    }
}

#[derive(Debug, PartialEq)]
pub struct Document {
    pub version: Numeric,
    pub graph: Graph,
}

parser! {
    fn document[I]()(I) -> Document
        where [I: Stream<Item=char>]
    {
        (whitespace(),
         lex(version()),
         lex(graph()))
            .map(|t| Document{ version: t.1, graph: t.2})
    }
}

#[derive(Debug, PartialEq)]
pub struct Graph {
    pub name: Ident,
    pub inputs: Vec<Ident>,
    pub outputs: Vec<Ident>,
    pub body: Vec<Assignment>,
}

parser! {
    fn graph[I]()(I) -> Graph
        where [I: Stream<Item=char>]
    {
        let id_list = || sep_by(lex(identifier()), lex(token(',')));

        (lex(string("graph")),
         lex(identifier()),
         between(lex(token('(')), lex(token(')')), id_list()),
         lex(string("->")),
         between(lex(token('(')), lex(token(')')), id_list()),
         between(lex(token('{')), token('}'),
                 many(lex(assignment())))
        ).map( |t| Graph{name: t.1, inputs: t.2, outputs: t.4, body: t.5})
    }
}

#[derive(Debug, PartialEq)]
pub struct Numeric {
    pub int: String,
    pub frac: Option<String>,
    pub exp: Option<String>,
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
    pub name: Ident,
    pub args: Vec<Argument>,
}

#[derive(Debug, PartialEq)]
pub struct Assignment {
    pub lexpr: LvalueExpr,
    pub invoc: Invocation,
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
            between(token('\''), token('\''),
                    many(satisfy(|c| c != '\''))).map(Literal::String),
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
                sep_by(whitespace().with(lex(rvalue_expr())), token(',')))
            .map(|v| RvalueExpr::Array(v))
    }
}

parser! {
    fn tuple_rvalue_expr[I]()(I) -> RvalueExpr
        where[I: Stream<Item=char>]
    {
        between(token('('), token(')'),
                sep_by(whitespace().with(lex(rvalue_expr())), token(',')))
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
                sep_by(whitespace().with(lex(lvalue_expr())), token(',')))
            .map(|v| LvalueExpr::Array(v))
    }
}

parser! {
    fn tuple_lvalue_expr[I]()(I) -> LvalueExpr
        where[I: Stream<Item=char>]
    {
        let with_paren = between(token('('), token(')'),
                sep_by(whitespace().with(lex(lvalue_expr())), token(',')))
            .map(|v| LvalueExpr::Tuple(v));

        // TODO: why match failed?
        // let without_paren = (identifier().map(LvalueExpr::Id),
        //                      many1(whitespace().with(token(',')).skip(whitespace())
        //                            .with(identifier().map(LvalueExpr::Id))))

        let without_paren = (lex(identifier()).map(LvalueExpr::Id),
                             many1(lex(token(','))
                                   .with(lex(identifier()).map(LvalueExpr::Id))))
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
        let named = (lex(identifier()),
                     lex(token('=')),
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
        (lex(identifier()),
         between(lex(token('(')), token(')'),
                 sep_by(lex(argument()), lex(token(',')))))
            .map(|t| Invocation{name: t.0, args: t.1})
    }
}

parser ! {
    fn assignment[I]()(I) -> Assignment
        where [I: Stream<Item=char>]
    {
        (lex(lvalue_expr()),
         lex(token('=')),
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
            Ok((LvalueExpr::Id(Ident::new("hoge")), ""))
        );

        assert_eq!(
            lvalue_expr().parse("hoge, fuga"),
            Ok((
                LvalueExpr::Tuple(vec![
                    LvalueExpr::Id(Ident::new("hoge")),
                    LvalueExpr::Id(Ident::new("fuga")),
                ]),
                "",
            ))
        );

        assert_eq!(
            lvalue_expr().parse("a1, a2 ="),
            Ok((
                LvalueExpr::Tuple(vec![
                    LvalueExpr::Id(Ident::new("a1")),
                    LvalueExpr::Id(Ident::new("a2")),
                ]),
                "=",
            ))
        );
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

        assert_eq!(
            assignment().parse("a1, a2 = fuga ( foo );"),
            Ok((
                Assignment {
                    lexpr: LvalueExpr::Tuple(vec![
                        LvalueExpr::Id(Ident::new("a1")),
                        LvalueExpr::Id(Ident::new("a2")),
                    ]),
                    invoc: Invocation {
                        name: Ident::new("fuga"),
                        args: vec![Argument::Rval(RvalueExpr::Id(Ident::new("foo")))],
                    },
                },
                "",
            ))
        );
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

        assert_eq!(
            literal().parse("'test'"),
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

    #[test]
    fn document_test() {
        assert_eq!(
            document().easy_parse(
                r#"
version 1

graph g (i) -> (o) {}
"#,
            ),
            Ok((
                Document {
                    version: Numeric::new("1"),
                    graph: Graph {
                        name: Ident::new("g"),
                        inputs: vec![Ident::new("i")],
                        outputs: vec![Ident::new("o")],
                        body: vec![],
                    },
                },
                "",
            ))
        );
    }

static ALEXNET: &'static str = r#"
version 1
graph AlexNet( input ) -> ( output )
{
    conv1 = conv(input, kernel1, bias1, padding = [(0,0), (0,0)],
                 border = 'constant', stride = [4, 4], dilation = [1, 1])
    relu1 = relu(conv1)
    pool1 = max_pool(relu1, size = [1, 1, 3, 3], stride = [1, 1, 2, 2],
                     border = 'ignore', padding = [(0,0), (0,0), (0,0), (0,0)])
    kernel2 = variable(shape = [192, 64, 5, 5], label = 'alexnet_v2/conv2/kernel')
    bias2 = variable(shape = [1, 192], label = 'alexnet_v2/conv2/bias')
    conv2 = conv(pool1, kernel2, bias2, padding = [(2,2), (2,2)],
                 border = 'constant', stride = [1, 1], dilation = [1, 1])
    relu2 = relu(conv2)
    pool2 = max_pool(relu2, size = [1, 1, 3, 3], stride = [1, 1, 2, 2],
                     border = 'ignore', padding = [(0,0), (0,0), (0,0), (0,0)])
    kernel3 = variable(shape = [384, 192, 3, 3], label = 'alexnet_v2/conv3/kernel')
    bias3 = variable(shape = [1, 384], label = 'alexnet_v2/conv3/bias')
    conv3 = conv(pool2, kernel3, bias3, padding = [(1,1), (1,1)],
                 border = 'constant', stride = [1, 1], dilation = [1, 1])
    relu3 = relu(conv3)
    kernel4 = variable(shape = [384, 384, 3, 3], label = 'alexnet_v2/conv4/kernel')
    bias4 = variable(shape = [1, 384], label = 'alexnet_v2/conv4/bias')
    conv4 = conv(relu3, kernel4, bias4, padding = [(1,1), (1,1)],
                 border = 'constant', stride = [1, 1], dilation = [1, 1])
    relu4 = relu(conv4)
    kernel5 = variable(shape = [256, 384, 3, 3], label = 'alexnet_v2/conv5/kernel')
    bias5 = variable(shape = [1, 256], label = 'alexnet_v2/conv5/bias')
    conv5 = conv(relu4, kernel5, bias5, padding = [(1,1), (1,1)],
                 border = 'constant', stride = [1, 1], dilation = [1, 1])
    relu5 = relu(conv5)
    pool3 = max_pool(relu5, size = [1, 1, 3, 3], stride = [1, 1, 2, 2],
                     border = 'ignore', padding = [(0,0), (0,0), (0,0), (0,0)])
    kernel6 = variable(shape = [4096, 256, 5, 5], label = 'alexnet_v2/fc6/kernel')
    bias6 = variable(shape = [1, 4096], label = 'alexnet_v2/fc6/bias')
    conv6 = conv(pool3, kernel6, bias6, padding = [(0,0), (0,0)],
                 border = 'constant', stride = [1, 1], dilation = [1, 1])
    relu6 = relu(conv6)
    kernel7 = variable(shape = [4096, 4096, 1, 1], label = 'alexnet_v2/fc7/kernel')
    bias7 = variable(shape = [1, 4096], label = 'alexnet_v2/fc7/bias')
    conv7 = conv(relu6, kernel7, bias7, padding = [(0,0), (0,0)],
                 border = 'constant', stride = [1, 1], dilation = [1, 1])
    relu7 = relu(conv7)
    kernel8 = variable(shape = [1000, 4096, 1, 1], label = 'alexnet_v2/fc8/kernel')
    bias8 = variable(shape = [1, 1000], label = 'alexnet_v2/fc8/bias')
    conv8 = conv(relu7, kernel8, bias8, padding = [(0,0), (0,0)],
                 border = 'constant', stride = [1, 1], dilation = [1, 1])
    output = softmax(conv8)
}
"#;

    #[test]
    fn alexnet_flat_test() {
        match parse_doc(ALEXNET) {
            Ok(_) => (),
            err => {
                println!("{:?}", err);
                panic!("ALEXNET parse failed")
            }
        }
    }

}
