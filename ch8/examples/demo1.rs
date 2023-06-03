use nom::branch::alt;
use nom::bytes::complete::{tag, take};
use nom::character::complete;
use nom::combinator::map;
use nom::multi::many1;
use nom::sequence::tuple;
use nom::IResult;

fn parse_abc(input: &str) -> IResult<&str, char> {
    alt((
        complete::char('a'),
        complete::char('b'),
        complete::char('c'),
    ))(input)
}

fn parse_abc_sequence(input: &str) -> IResult<&str, (char, char, char)> {
    tuple((
        complete::char('a'),
        complete::char('b'),
        complete::char('c'),
    ))(input)
}

fn parse_abc_string(input: &str) -> IResult<&str, &str> {
    tag("abc")(input)
}

fn parse_abc_as_number(input: &str) -> IResult<&str, u8> {
    alt((
        map(complete::char('a'), |_| 5),
        map(complete::char('b'), |_| 16),
        map(complete::char('c'), |_| 8),
    ))(input)
}

fn parse_abc_to_ac(input: &str) -> IResult<&str, (char, char)> {
    tuple((
        complete::char('a'),
        complete::char('b'),
        complete::char('c'),
    ))(input)
    .map(|(rest, result)| (rest, (result.0, result.2)))
}

fn parse_variable_text(input: &str) -> IResult<&str, (char, &str)> {
    tuple((complete::char('n'), take(2usize)))(input)
}

fn repeated_text(input: &str) -> IResult<&str, Vec<&str>> {
    many1(take(3usize))(input)
}

fn main() {
    println!("a:{:?}", parse_abc("a"));
    println!("x:{:?}", parse_abc("x"));
    println!("bjk:{:?}", parse_abc("bjk"));
    println!("abce:{:?}", parse_abc_sequence("abce"));
    println!("abc dd:{:?}", parse_abc_string("abc dd"));
    println!("aa:{:?}", parse_abc_as_number("aa"));
    println!("abc:{:?}", parse_abc_to_ac("abc"));
    println!("nabc:{:?}", parse_variable_text("nabc"));
    println!("abcdd:{:?}", repeated_text("abcdd"));
}
