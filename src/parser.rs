use nom::bytes::complete::*;
use nom::character::complete::*;
use nom::error::{ParseError, VerboseError};
use nom::multi::many0;
use nom::sequence::{delimited, tuple};
use nom::Parser;
use nom::{bytes::complete::tag, combinator::opt, multi::separated_list0, IResult};
use nom_locate::position;

use crate::data::*;

pub type ParserResult<'a, Out> = IResult<Span<'a>, Out, VerboseError<Span<'a>>>;

pub fn module(input: Span) -> ParserResult<Module> {
	let mut actions = Vec::new();
	let mut classes = Vec::new();

	let mut input = input;

	loop{
		let action_result = action(input);
		let fail_action = action_result.is_err();
		if let Ok((rest_input, action)) = action_result {
			input = rest_input;
			actions.push(action);
		}
		let class_result = class(input);
		let fail_class = class_result.is_err();
		if let Ok((rest_input, class)) = class_result {
			input = rest_input;
			classes.push(class);
		}
		if fail_action && fail_class {
			break;
		}
	}

	let imports = actions
	.iter()
	.filter_map(|elem| {
		if let Action::ImportStmt(import) = elem {
			Some(import.clone())
		} else {
			None
		}
	})
	.collect();


	Ok((
		input,
		Module {
			actions,
			classes,
			imports
		},
	))
}

pub fn action(input: Span) -> ParserResult<Action> {
	as_action(import)(input)
}

pub fn import(input: Span) -> ParserResult<ImportStmt> {
	let (input, import) = tag("import")(input)?;
	let (input, _) = multispace0(input)?;
	let (input, path) = quote_string(input)?;
	let (input, _) = multispace0(input)?;
	let (input, for_token) = tag("for")(input)?;
	let (input, _) = multispace1(input)?;
	let (input, variables) =
		separated_list0(tuple((multispace0, tag(","), multispace0)), import_variable)(input)?;
	let (input, _) = whitespace0(input)?;
	let (input, _) = line_ending_or_eof(input)?;
	Ok((input, ImportStmt { import_token: import.into(), path: path.into(), for_token: for_token.into(), variables }))
}

pub fn class(input: Span) -> ParserResult<Class> {
	let (input, _) = multispace0(input)?;
	let (input, attributes) = many0(attribute)(input)?;
	let (input, _) = multispace0(input)?;
	let (input, class_specifier) = tag("class")(input)?;
	let (input, _) = multispace1(input)?;
	let (input, name) = alphanumeric1(input)?;
	let (input, is_superclass) = opt(tuple((multispace1, tag("is"), multispace1, alphanumeric1, multispace0)))(input)?;
	Ok((input, Class{ 
		attributes, 
		name: name.into(),
		class_keyword: class_specifier.into(), 
		superclass: is_superclass.map(|is_soup| is_soup.3.into()), 
		methods: Vec::new()}))
}

pub fn attribute(input: Span) -> ParserResult<Attribute> {
	let (input, _) = multispace0(input)?;
	let (input, tag_char) = tag("#")(input)?;
	let (input, _) = multispace0(input)?;
	let (input, runtime_specifier) = opt(tag("!"))(input)?;
	let (input, _) = multispace0(input)?;
	let (input, key) = alphanumeric1(input)?;
	let (input, _) = multispace0(input)?;
	let (input, attribute) = attribute_value(input)?; //todo: parse subattributes
	let (input, _) = multispace0(input)?;

	Ok((input, Attribute{
		name: key.into(), 
		tag: tag_char.into(), 
		runtime_specifier: runtime_specifier.map(|s|s.into()), 
		value: AttributeValue::Expr(attribute)}))
}

pub fn attribute_value(input: Span) -> ParserResult<Expr> {
	let (input, _) = char('=')(input)?;
	let (input, _) = multispace0(input)?;
	as_expr(string_expr)(input)
}

pub fn import_variable(input: Span) -> ParserResult<ImportVar> {
	let (input, var_tokens) = tuple((
		opt(tuple((alphanumeric1, multispace1, tag("as"), multispace1))),
		alphanumeric1,
	))(input)?;
	let name: Token = var_tokens.1.into();
	let source_as = var_tokens.0;
	let as_keyword = source_as.map(|as_kw| as_kw.2.into());
	let source = source_as.map_or(name.clone(), |as_kw| as_kw.0.into());
	Ok((input, ImportVar { name, as_keyword, source }))
}

pub fn as_expr<'a, I, O, E: ParseError<I>, F>(
	first: F,
) -> impl FnMut(I) -> IResult<I, Expr<'a>, E>
where
	F: Parser<I, O, E>,
	O: std::convert::Into<Expr<'a>>,
{
	as_other::<_, _, Expr, _, _>(first)
}

pub fn as_action<'a, I, O, E: ParseError<I>, F>(
	first: F,
) -> impl FnMut(I) -> IResult<I, Action<'a>, E>
where
	F: Parser<I, O, E>,
	O: std::convert::Into<Action<'a>>,
{
	as_other::<_, _, Action, _, _>(first)
}

pub fn as_stmt<'a, I, O, E: ParseError<I>, F>(
	first: F,
) -> impl FnMut(I) -> IResult<I, Stmt<'a>, E>
where
	F: Parser<I, O, E>,
	O: std::convert::Into<Stmt<'a>>,
{
	as_other::<_, _, Stmt, _, _>(first)
}

fn as_other<I, O, T, E: ParseError<I>, F>(
	mut first: F,
) -> impl FnMut(I) -> IResult<I, T, E>
where
	F: Parser<I, O, E>,
	O: std::convert::Into<T>,
{
	move |input: I| {
		let (input, result) = first.parse(input)?;
		let result = result.into();
		Ok((input, result))
	}
}

pub fn string_expr(input: Span) -> ParserResult<StringExpr> {
	let (input, string) = quote_string(input)?;
	let expr = StringExpr { string: string.into() };
	Ok((input, expr))
}

//todo: support string interpolation and multiline strings
pub fn quote_string(input: Span) -> ParserResult<Span> {
	let (input, string) =
		delimited(char('"'), escaped(is_not("\""), '\\', one_of("\\\"")), char('"'))(input)?;
	Ok((input, string))
}

//similar to multispace0, but doesnt eat newlines
pub fn whitespace0(input: Span) -> ParserResult<Span> {
	take_while(|c: char| c == ' ' || c == '\t')(input)
}

//similar to multispace1, but doesnt eat newlines
pub fn whitespace1(input: Span) -> ParserResult<Span> {
	take_while1(|c: char| c == ' ' || c == '\t')(input)
}

pub fn line_ending_or_eof(input: Span) -> ParserResult<Span> {
	if input.is_empty() {
		return position(input);
	}
	line_ending(input)
}