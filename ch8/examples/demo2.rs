use crate::analyzer::analyze_program;
use crate::symbol_table::SymbolTable;
use std::env::{args, var};
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
                let parsed_program = syntax_tree;
                let mut variables = SymbolTable::new();
                match analyze_program(&mut variables, &parsed_program) {
                    Ok(analyzed_tree) => {
                        println!("Symbol table:{variables:?}");
                        println!("analyzed program:{analyzed_tree:#?}");
                    }
                    Err(err) => {
                        eprintln!("invalid code in {}", err);
                        return;
                    }
                }
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

mod symbol_table {

    #[derive(Debug)]
    pub struct SymbolTable {
        entries: Vec<(String, f64)>,
    }

    impl SymbolTable {
        pub fn new() -> Self {
            Self {
                entries: Default::default(),
            }
        }

        pub fn insert_symbol(&mut self, identifier: &str) -> Result<usize, String> {
            if self
                .entries
                .iter()
                .find(|(item, _)| item == identifier)
                .is_some()
            {
                Err(format!(
                    "Error:Identifier{} declared serveral times",
                    identifier
                ))
            } else {
                self.entries.push((identifier.to_string(), 0.0));
                Ok(self.entries.len())
            }
        }
        pub fn find_symbol(&self, identifier: &str) -> Result<usize, String> {
            if let Some(pos) = self.entries.iter().position(|(item, _)| item == identifier) {
                Ok(pos)
            } else {
                Err(format!(
                    "Error:Identifier {} used before having been declared",
                    identifier
                ))
            }
        }
    }
}

mod analyzer {
    use crate::parser::{
        ExprOperator, ParsedExpr, ParsedFactor, ParsedProgram, ParsedStatement, ParsedTerm,
        TermOperator,
    };
    use crate::symbol_table::SymbolTable;

    #[derive(Debug, PartialEq)]
    pub enum AnalyzedFactor {
        Literal(f64),
        Identifier(usize),
        SubExpression(Box<AnalyzedExpr>),
    }

    pub type AnalyzedTerm = (AnalyzedFactor, Vec<(TermOperator, AnalyzedFactor)>);

    pub type AnalyzedExpr = (AnalyzedTerm, Vec<(ExprOperator, AnalyzedTerm)>);

    #[derive(Debug)]
    pub enum AnalyzedStatement {
        Declaration(usize),
        InputOperation(usize),
        OutputOperation(AnalyzedExpr),
        Assignment(usize, AnalyzedExpr),
    }

    pub type AnalyzedProgram = Vec<AnalyzedStatement>;

    pub fn analyze_program(
        variables: &mut SymbolTable,
        parsed_program: &ParsedProgram,
    ) -> Result<AnalyzedProgram, String> {
        let mut analyzed_program = AnalyzedProgram::new();
        for statement in parsed_program {
            analyzed_program.push(analyze_statement(variables, statement)?);
        }
        Ok(analyzed_program)
    }

    fn analyze_factor(
        variables: &mut SymbolTable,
        parsed_factor: &ParsedFactor,
    ) -> Result<AnalyzedFactor, String> {
        match parsed_factor {
            ParsedFactor::Literal(value) => Ok(AnalyzedFactor::Literal(*value)),
            ParsedFactor::Identifier(name) => {
                Ok(AnalyzedFactor::Identifier(variables.find_symbol(name)?))
            }
            ParsedFactor::SubExpression(expr) => {
                Ok(AnalyzedFactor::SubExpression(Box::<AnalyzedExpr>::new(
                    analyze_expr(variables, expr)?,
                )))
            }
        }
    }

    fn analyze_term(
        variables: &mut SymbolTable,
        parsed_term: &ParsedTerm,
    ) -> Result<AnalyzedTerm, String> {
        let first_factor = analyze_factor(variables, &parsed_term.0)?;
        let mut other_factors = Vec::<(TermOperator, AnalyzedFactor)>::new();
        for factor in &parsed_term.1 {
            other_factors.push((factor.0, analyze_factor(variables, &factor.1)?));
        }
        Ok((first_factor, other_factors))
    }

    fn analyze_expr(
        variables: &mut SymbolTable,
        parsed_expr: &ParsedExpr,
    ) -> Result<AnalyzedExpr, String> {
        let first_term = analyze_term(variables, &parsed_expr.0)?;
        let mut other_terms = Vec::<(ExprOperator, AnalyzedTerm)>::new();
        for term in &parsed_expr.1 {
            other_terms.push((term.0, analyze_term(variables, &term.1)?));
        }
        Ok((first_term, other_terms))
    }

    fn analyze_statement(
        variables: &mut SymbolTable,
        parsed_statement: &ParsedStatement,
    ) -> Result<AnalyzedStatement, String> {
        match parsed_statement {
            ParsedStatement::Assignment(identifier, expr) => {
                let handle = variables.find_symbol(identifier)?;
                let analyzed_expr = analyze_expr(variables, expr)?;
                Ok(AnalyzedStatement::Assignment(handle, analyzed_expr))
            }
            ParsedStatement::Declaration(identifier) => {
                let handle = variables.insert_symbol(identifier)?;
                Ok(AnalyzedStatement::Declaration(handle))
            }
            ParsedStatement::InputOperation(identifier) => {
                let handle = variables.find_symbol(identifier)?;
                Ok(AnalyzedStatement::InputOperation(handle))
            }
            ParsedStatement::OutputOperation(expr) => {
                let analyzed_expr = analyze_expr(variables, expr)?;
                Ok(AnalyzedStatement::OutputOperation(analyzed_expr))
            }
        }
    }
}
