use std::env::args;
use std::fs;

fn main() {
    let mut args = args();
    let _ = args.next();
    match args.next() {
        Some(path) => {
            if path.ends_with(".calc") {
                process_file(&path);
            } else {
                println!("is not calc program file");
            }
        }
        None => println!("path arg is missing"),
    }
}

fn process_file(path: &str) {
    match fs::read_to_string(path) {
        Ok(source_code) => match parser::parse_program(&source_code) {
            Ok((rest, syntax_tree)) => {
                let trimmed_rest = rest.trim();
                if trimmed_rest.len() > 0 {
                    eprintln!("Invalid remaining code in {}", trimmed_rest);
                    return;
                }
                println!("Parsed program: {:#?}", parsed_program = syntax_tree);
            }
            Err(err) => {
                eprintln!("Invalid code in : {:?}", err);
                return;
            }
        },
        Err(_) => println!("failed to read file:{path}"),
    }
}

mod parser {
    use crate::process_file;
    use nom::branch::alt;
    use nom::bytes::complete::{tag, take_while};
    use nom::character::complete;
    use nom::character::complete::alpha1;
    use nom::combinator::{complete, map};
    use nom::multi::many0;
    use nom::number::complete::double;
    use nom::sequence::{delimited, preceded, tuple};
    use nom::IResult;

    #[derive(Debug, PartialEq)]
    pub enum ParsedFactor<'a> {
        Literal(f64),
        Identifier(&'a str),
        SubExpression(Box<ParsedExpr<'a>>),
    }

    #[derive(Debug, PartialEq, Clone, Copy)]
    pub enum TermOperator {
        Multiply,
        Divide,
    }

    #[derive(Debug, PartialEq, Clone, Copy)]
    pub enum ExprOperator {
        Add,
        Subtract,
    }

    pub type ParsedTerm<'a> = (ParsedFactor<'a>, Vec<(TermOperator, ParsedFactor<'a>)>);

    pub type ParsedExpr<'a> = (ParsedTerm<'a>, Vec<(ExprOperator, ParsedTerm<'a>)>);

    #[derive(Debug)]
    pub enum ParsedStatement<'a> {
        Declaration(&'a str),
        InputOperation(&'a str),
        OutputOperation(ParsedExpr<'a>),
        Assignment(&'a str, ParsedExpr<'a>),
    }

    pub type ParsedProgram<'a> = Vec<ParsedStatement<'a>>;

    pub fn parse_declaration(input: &str) -> IResult<&str, ParsedStatement> {
        tuple((complete::char('@'), skip_spaces, parse_identifier))(input)
            .map(|(input, output)| (input, ParsedStatement::Declaration(output.2)))
    }

    pub fn parse_input_statement(input: &str) -> IResult<&str, ParsedStatement> {
        tuple((complete::char('>'), skip_spaces, parse_identifier))(input)
            .map(|(input, output)| (input, ParsedStatement::InputOperation(output.2)))
    }

    pub fn parse_output_statement(input: &str) -> IResult<&str, ParsedStatement> {
        tuple((complete::char('<'), skip_spaces, parse_expr))(input)
            .map(|(input, output)| (input, ParsedStatement::OutputOperation(output.2)))
    }

    fn parse_expr(input: &str) -> IResult<&str, ParsedExpr> {
        tuple((
            parse_term,
            many0(tuple((
                preceded(
                    skip_spaces,
                    alt((
                        map(complete::char('+'), |_| ExprOperator::Add),
                        map(complete::char('-'), |_| ExprOperator::Subtract),
                    )),
                ),
                parse_term,
            ))),
        ))(input)
    }

    fn parse_term(input: &str) -> IResult<&str, ParsedTerm> {
        tuple((
            parse_factor,
            many0(tuple((
                preceded(
                    skip_spaces,
                    alt((
                        map(complete::char('*'), |_| TermOperator::Multiply),
                        map(complete::char('/'), |_| TermOperator::Divide),
                    )),
                ),
                parse_factor,
            ))),
        ))(input)
    }

    fn parse_factor(input: &str) -> IResult<&str, ParsedFactor> {
        preceded(
            skip_spaces,
            alt((
                map(parse_identifier, ParsedFactor::Identifier),
                map(double, ParsedFactor::Literal),
                map(parse_subexpr, |expr| {
                    ParsedFactor::SubExpression(Box::new(expr))
                }),
            )),
        )(input)
    }

    fn parse_subexpr(input: &str) -> IResult<&str, ParsedExpr> {
        delimited(
            preceded(skip_spaces, complete::char('(')),
            parse_expr,
            preceded(skip_spaces, complete::char(')')),
        )(input)
    }

    pub fn skip_spaces(input: &str) -> IResult<&str, &str> {
        let chars = " \t\r\n";
        take_while(move |ch| chars.contains(ch))(input)
    }

    pub fn parse_identifier(input: &str) -> IResult<&str, &str> {
        alpha1(input)
    }
    fn parse_assignment(input: &str) -> IResult<&str, ParsedStatement> {
        tuple((
            parse_identifier,
            skip_spaces,
            tag(":="),
            skip_spaces,
            parse_expr,
        ))(input)
        .map(|(input, output)| (input, ParsedStatement::Assignment(output.0, output.4)))
    }

    pub fn parse_program(input: &str) -> IResult<&str, ParsedProgram> {
        many0(preceded(
            skip_spaces,
            alt((
                parse_declaration,
                parse_input_statement,
                parse_output_statement,
                parse_assignment,
            )),
        ))(input)
    }
}
