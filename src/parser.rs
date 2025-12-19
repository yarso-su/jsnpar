//! # JSON Parser implementation using parser combinators
//!
//! This is an educational project not intended to be used in a production environment. Although I
//! plan to improve the behaviour and implementation. Feel free to use it to make your own
//! implementation, or take whatever you can leverage.
//!
//! I created this crate thanks to Bodil's article on [parser combinators](https://bodil.lol/parser-combinators/).

use super::JsonValue;
use std::collections::HashMap;

/// Utility type for parsing results. It is a `Result` with the remaining input and the parsed value.
pub type ParseResult<'a, Output> = Result<(&'a str, Output), &'a str>;

/// Struct to allow Rust to calculate type sizes at runtime to avoid stack overflows.
pub struct BoxedParser<'a, Output> {
    parser: Box<dyn Parser<'a, Output> + 'a>,
}

fn map<'a, P, F, A, B>(parser: P, map_fn: F) -> impl Parser<'a, B>
where
    P: Parser<'a, A>,
    F: Fn(A) -> B,
{
    move |input| {
        parser
            .parse(input)
            .map(|(next_input, result)| (next_input, map_fn(result)))
    }
}

fn pred<'a, P, A, F>(parser: P, predicate: F) -> impl Parser<'a, A>
where
    P: Parser<'a, A>,
    F: Fn(&A) -> bool,
{
    move |input| {
        if let Ok((next_input, value)) = parser.parse(input)
            && predicate(&value)
        {
            return Ok((next_input, value));
        }
        Err(input)
    }
}

/// Trait for parsers. Comes with a few useful methods to make it easier to connect parsers.
pub trait Parser<'a, Output> {
    fn parse(&self, input: &'a str) -> ParseResult<'a, Output>;

    fn map<F, NewOutput>(self, map_fn: F) -> BoxedParser<'a, NewOutput>
    where
        Self: Sized + 'a,
        Output: 'a,
        NewOutput: 'a,
        F: Fn(Output) -> NewOutput + 'a,
    {
        BoxedParser::new(map(self, map_fn))
    }

    fn pred<F>(self, pred_fn: F) -> BoxedParser<'a, Output>
    where
        Self: Sized + 'a,
        Output: 'a,
        F: Fn(&Output) -> bool + 'a,
    {
        BoxedParser::new(pred(self, pred_fn))
    }
}

impl<'a, F, Output> Parser<'a, Output> for F
where
    F: Fn(&'a str) -> ParseResult<Output>,
{
    fn parse(&self, input: &'a str) -> ParseResult<'a, Output> {
        self(input)
    }
}

impl<'a, Output> BoxedParser<'a, Output> {
    fn new<P>(parser: P) -> Self
    where
        P: Parser<'a, Output> + 'a,
    {
        BoxedParser {
            parser: Box::new(parser),
        }
    }
}

impl<'a, Output> Parser<'a, Output> for BoxedParser<'a, Output> {
    fn parse(&self, input: &'a str) -> ParseResult<'a, Output> {
        self.parser.parse(input)
    }
}

fn match_literal<'a>(expected: &'static str) -> impl Parser<'a, ()> {
    move |input: &'a str| {
        input
            .strip_prefix(expected)
            .map(|rest| (rest, ()))
            .ok_or(input)
    }
}

fn pair<'a, P1, P2, R1, R2>(parser1: P1, parser2: P2) -> impl Parser<'a, (R1, R2)>
where
    P1: Parser<'a, R1>,
    P2: Parser<'a, R2>,
{
    move |input| {
        parser1.parse(input).and_then(|(next_input, result1)| {
            parser2
                .parse(next_input)
                .map(|(last_input, result2)| (last_input, (result1, result2)))
        })
    }
}

fn left<'a, P1, P2, R1, R2>(parser1: P1, parser2: P2) -> impl Parser<'a, R1>
where
    P1: Parser<'a, R1>,
    P2: Parser<'a, R2>,
{
    map(pair(parser1, parser2), |(left, _right)| left)
}

fn right<'a, P1, P2, R1, R2>(parser1: P1, parser2: P2) -> impl Parser<'a, R2>
where
    P1: Parser<'a, R1>,
    P2: Parser<'a, R2>,
{
    map(pair(parser1, parser2), |(_left, right)| right)
}

fn either<'a, P1, P2, A>(parser1: P1, parser2: P2) -> impl Parser<'a, A>
where
    P1: Parser<'a, A>,
    P2: Parser<'a, A>,
{
    move |input| parser1.parse(input).or_else(|_| parser2.parse(input))
}

fn optional<'a, P, A>(parser: P) -> impl Parser<'a, Option<A>>
where
    P: Parser<'a, A>,
{
    move |input| match parser.parse(input) {
        Ok((remaining, value)) => Ok((remaining, Some(value))),
        Err(_) => Ok((input, None)),
    }
}

fn zero_or_more<'a, P, A>(parser: P) -> impl Parser<'a, Vec<A>>
where
    P: Parser<'a, A>,
{
    move |mut input| {
        let mut results = Vec::new();

        while let Ok((next_input, next_item)) = parser.parse(input) {
            input = next_input;
            results.push(next_item);
        }

        Ok((input, results))
    }
}

pub fn one_or_more<'a, P, A>(parser: P) -> impl Parser<'a, Vec<A>>
where
    P: Parser<'a, A>,
{
    move |mut input| {
        let mut results = Vec::new();

        if let Ok((next_input, result)) = parser.parse(input) {
            input = next_input;
            results.push(result);
        } else {
            return Err(input);
        }

        while let Ok((next_input, next_item)) = parser.parse(input) {
            input = next_input;
            results.push(next_item);
        }

        Ok((input, results))
    }
}

fn any_char<'a>(input: &'a str) -> ParseResult<'a, char> {
    match input.chars().next() {
        Some(next) => Ok((&input[next.len_utf8()..], next)),
        _ => Err(input),
    }
}

fn whitespace_char<'a>() -> impl Parser<'a, char> {
    any_char.pred(|c| c.is_whitespace())
}

fn space0<'a>() -> impl Parser<'a, Vec<char>> {
    zero_or_more(whitespace_char())
}

fn whitespace_wrap<'a, P, A>(parser: P) -> impl Parser<'a, A>
where
    P: Parser<'a, A>,
{
    right(space0(), left(parser, space0()))
}

fn quoted_string<'a>() -> impl Parser<'a, String> {
    right(
        match_literal("\""),
        left(
            zero_or_more(any_char.pred(|c| *c != '"')),
            match_literal("\""),
        ),
    )
    .map(|chars| chars.into_iter().collect())
}

fn boolean_value<'a>() -> impl Parser<'a, JsonValue> {
    either(
        match_literal("true").map(|_| JsonValue::Bool(true)),
        match_literal("false").map(|_| JsonValue::Bool(false)),
    )
}

fn string_value<'a>() -> impl Parser<'a, JsonValue> {
    quoted_string().map(JsonValue::String)
}

fn number_value<'a>() -> impl Parser<'a, JsonValue> {
    map(
        pair(
            optional(match_literal("-")),
            pair(
                one_or_more(any_char.pred(|c| c.is_numeric())),
                optional(right(
                    match_literal("."),
                    one_or_more(any_char.pred(|c| c.is_numeric())),
                )),
            ),
        ),
        |(sign, (int_part, decimal_part))| {
            let mut num_str = String::new();

            if sign.is_some() {
                num_str.push('-');
            }

            num_str.push_str(&int_part.iter().collect::<String>());

            if let Some(dec) = decimal_part {
                num_str.push('.');
                num_str.push_str(&dec.iter().collect::<String>());
            }

            JsonValue::Number(num_str.parse::<f64>().unwrap())
        },
    )
}

fn null_value<'a>() -> impl Parser<'a, JsonValue> {
    match_literal("null").map(|_| JsonValue::Null)
}

fn last_array_value<'a>() -> BoxedParser<'a, Option<JsonValue>> {
    BoxedParser::new(either(
        match_literal("]").map(|_| None),
        left(
            whitespace_wrap(json_value()),
            whitespace_wrap(match_literal("]")),
        )
        .map(Some),
    ))
}

fn last_object_value<'a>() -> BoxedParser<'a, Option<(String, JsonValue)>> {
    BoxedParser::new(either(
        match_literal("}").map(|_| None),
        pair(
            whitespace_wrap(quoted_string()),
            right(
                whitespace_wrap(match_literal(":")),
                left(
                    whitespace_wrap(json_value()),
                    whitespace_wrap(match_literal("}")),
                ),
            ),
        )
        .map(|(key, value)| Some((key, value))),
    ))
}

pub fn array_value<'a>() -> BoxedParser<'a, JsonValue> {
    BoxedParser::new(move |input| {
        whitespace_wrap(right(
            match_literal("["),
            pair(
                zero_or_more(left(
                    whitespace_wrap(json_value()),
                    whitespace_wrap(match_literal(",")),
                ))
                .map(JsonValue::Array),
                last_array_value(),
            )
            .map(|(arr, last)| {
                if let JsonValue::Array(mut arr) = arr {
                    if let Some(last) = last {
                        arr.push(last);
                    }
                    JsonValue::Array(arr)
                } else {
                    arr
                }
            }),
        ))
        .parse(input)
    })
}

fn object_value<'a>() -> BoxedParser<'a, JsonValue> {
    BoxedParser::new(move |input| {
        whitespace_wrap(right(
            match_literal("{"),
            pair(
                zero_or_more(pair(
                    whitespace_wrap(quoted_string()),
                    right(
                        whitespace_wrap(match_literal(":")),
                        left(
                            whitespace_wrap(json_value()),
                            whitespace_wrap(match_literal(",")),
                        ),
                    ),
                ))
                .map(|pairs| {
                    let mut hash = HashMap::new();
                    for (key, value) in pairs {
                        hash.insert(key, value);
                    }
                    JsonValue::Object(hash)
                }),
                last_object_value(),
            )
            .map(|(object, last)| {
                if let JsonValue::Object(mut hash) = object {
                    if let Some((key, value)) = last {
                        hash.insert(key, value);
                    }
                    JsonValue::Object(hash)
                } else {
                    object
                }
            }),
        ))
        .parse(input)
    })
}

/// Parses a JSON value. Use this as the entry point for parsing JSON.
pub fn json_value<'a>() -> BoxedParser<'a, JsonValue> {
    BoxedParser::new(move |input| {
        either(
            either(
                either(null_value(), boolean_value()),
                either(string_value(), number_value()),
            ),
            either(object_value(), array_value()),
        )
        .parse(input)
    })
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn literal_parser() {
        let hello = match_literal("hello");

        assert_eq!(hello.parse("hello"), Ok(("", ())));
        assert_eq!(hello.parse("hello world"), Ok((" world", ())));
        assert_eq!(hello.parse("world"), Err("world"));
    }

    #[test]
    fn pair_combinator() {
        let opener = pair(match_literal("{"), match_literal(r#""hello""#));

        assert_eq!(
            opener.parse(r#"{"hello":"world"}"#),
            Ok((r#":"world"}"#, ((), ()))),
        );
        assert_eq!(opener.parse("oops"), Err("oops"));
        assert_eq!(opener.parse("{oops"), Err("oops"));
    }

    #[test]
    fn right_combinator() {
        let opener = right(match_literal("{"), quoted_string());

        assert_eq!(
            opener.parse(r#"{"hello":"world"}"#),
            Ok((r#":"world"}"#, String::from("hello")))
        );
        assert_eq!(opener.parse("oops"), Err("oops"));
        assert_eq!(opener.parse("{oops"), Err("oops"));
    }

    #[test]
    fn left_combinator() {
        let opener = left(
            right(match_literal("{"), quoted_string()),
            match_literal(":"),
        );

        assert_eq!(
            opener.parse(r#"{"hello":"world"}"#),
            Ok((r#""world"}"#, String::from("hello")))
        );
        assert_eq!(opener.parse("oops"), Err("oops"));
        assert_eq!(opener.parse("{oops"), Err("oops"));
    }

    #[test]
    fn either_combinator() {
        let or_parser = either(match_literal("true"), match_literal("false"));

        assert_eq!(or_parser.parse("true"), Ok(("", ())));
        assert_eq!(or_parser.parse("false"), Ok(("", ())));
        assert_eq!(or_parser.parse("null"), Err("null"));
    }

    #[test]
    fn zero_or_more_combinator() {
        let array_parser = right(zero_or_more(match_literal(" ")), quoted_string());

        assert_eq!(
            array_parser.parse(r#"          "hello""#),
            Ok(("", String::from("hello"))),
        );
        assert_eq!(
            array_parser.parse(r#"          "hello"world"#),
            Ok(("world", String::from("hello"))),
        );
        assert_eq!(
            array_parser.parse(r#""hello""#),
            Ok(("", String::from("hello"))),
        );
    }

    #[test]
    fn object_parser() {
        assert_eq!(
            object_value().parse("{}"),
            Ok(("", JsonValue::Object(HashMap::new())))
        );
        assert_eq!(
            object_value().parse(
                r#"{"name":"joe","active":true, "nested": {"foo":"bar"}, "packages_weights": [1,-3.1416]}"#
            ),
            Ok((
                "",
                JsonValue::Object(HashMap::from([
                    ("name".to_string(), JsonValue::String("joe".to_string())),
                    ("active".to_string(), JsonValue::Bool(true)),
                    (
                        "nested".to_string(),
                        JsonValue::Object(HashMap::from([(
                            "foo".to_string(),
                            JsonValue::String("bar".to_string())
                        )]))
                    ),
                    (
                        "packages_weights".to_string(),
                        JsonValue::Array(vec![
                            JsonValue::Number(1.0),
                            #[allow(clippy::approx_constant)]
                            JsonValue::Number(-3.1416),
                        ])
                    )
                ]))
            ))
        );
    }

    #[test]
    fn json() {
        assert_eq!(
            json_value().parse(
                r#"[false,{"name":"joe","active":true, "nested": {"foo":"bar"}, "packages_weights": [1,-3.1416]}]"#
            ),
            Ok((
                "",
                JsonValue::Array(vec![
                    JsonValue::Bool(false),
                JsonValue::Object(HashMap::from([
                    ("name".to_string(), JsonValue::String("joe".to_string())),
                    ("active".to_string(), JsonValue::Bool(true)),
                    (
                        "nested".to_string(),
                        JsonValue::Object(HashMap::from([(
                            "foo".to_string(),
                            JsonValue::String("bar".to_string())
                        )]))
                    ),
                    (
                        "packages_weights".to_string(),
                        JsonValue::Array(vec![
                            JsonValue::Number(1.0),
                            #[allow(clippy::approx_constant)]
                            JsonValue::Number(-3.1416),
                        ])
                    )
                ]))
                ])
            ))
        );
    }
}
