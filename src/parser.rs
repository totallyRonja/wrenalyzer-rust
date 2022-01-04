use nom::branch::alt;
use nom::bytes::complete::*;
use nom::character::complete::*;
use nom::error::{ParseError, VerboseError};
use nom::multi::many0;
use nom::sequence::{tuple, delimited};
use nom::{bytes::complete::tag, combinator::opt, multi::separated_list0, IResult};
use nom::{Parser, Slice};
use nom_locate::position;

use crate::data::*;

pub type ParserResult<'a, Out> = IResult<Span<'a>, Out, VerboseError<Span<'a>>>;

pub fn module(input: Span) -> ParserResult<Module> {
	let mut actions = Vec::new();
	let mut classes:Vec<Class> = Vec::new();

	let mut input = input;

	loop {
		let action_result = action(input);
		let fail_action = action_result.is_err();
		if let Ok((rest_input, action)) = action_result {
			input = rest_input;
			actions.push(action);
		}
		let class_result = class(input);
		let fail_class = class_result.is_err();
		if let Ok((rest_input, class)) = &class_result {
			input = rest_input.to_owned();
			classes.push(class.to_owned());
		}
		if let Err(err) = &class_result {
			println!("{:#?}", err);
		}
		if fail_action && fail_class {
			break;
		}
	}

	let imports =
		actions
			.iter()
			.filter_map(|elem| {
				if let Action::ImportStmt(import) = elem {
					Some(import.clone())
				} else {
					None
				}
			})
			.collect();

	Ok((input, Module { actions, classes, imports }))
}

pub fn action(input: Span) -> ParserResult<Action> {
	alt((as_action(import), as_action(number), as_action(string_expr)))(input)
}

pub fn expr(input: Span) -> ParserResult<Expr> {
	alt((as_expr(number), as_expr(string_expr)))(input)
}

pub fn import(input: Span) -> ParserResult<ImportStmt> {
	let (input, before) = multispace0(input)?;
	let (input, import) = tag("import")(input)?;
	let import = Token::new_w_before(before, import);
	let (input, before_path) = multispace0(input)?;
	let (input, path) = quote_string(input)?;
	let (input, after_path) = multispace0(input)?;
	let path_token = Token::new(before_path, path, after_path);
	let (input, for_token) = tag("for")(input)?;
	let (input, after_for) = multispace1(input)?;
	let for_token = Token::new_w_after(for_token, after_for);
	let (input, variables) =
		separated_list0(tuple((multispace0, tag(","), multispace0)), import_variable)(input)?;
	let (input, _) = whitespace0(input)?;
	let (input, _) = line_ending_or_eof(input)?;
	Ok((input, ImportStmt { import_token: import, path: path_token, for_token, variables }))
}

pub fn number(input: Span) -> ParserResult<NumExpr> {
	let (input, before) = multispace0(input)?;
	let (input, prefix) = opt(tag("0x"))(input)?;
	let input_before_num = input;
	let (input, mut number) = digit1(input)?;
	let (input, fractional) = opt(tuple((char('.'), digit1)))(input)?;
	let (input, after) = whitespace0(input)?;
	let prefix = prefix.map(|p| Token::new_w_before(before, p));
	if let Some(frac) = fractional {
		number = input_before_num.slice(..(number.len() + 1 + frac.1.len()))
	}
	let number = if prefix.is_some() {
		Token::new_w_after(number, after)
	} else {
		Token::new(before, number, after)
	};
	Ok((input, NumExpr { prefix, number }))
}

pub fn class(input: Span) -> ParserResult<Class> {
	let (input, attributes) = many0(attribute)(input)?;
	let (input, _) = multispace0(input)?;
	let (input, class_specifier) = tag("class")(input)?;
	let (input, _) = multispace1(input)?;
	let (input, name) = alphanumeric1(input)?;
	let (input, is_superclass) =
		opt(tuple((multispace1, tag("is"), multispace1, alphanumeric1, multispace0)))(input)?;
	let (input, _) = multispace0(input)?;
	let (input, body) = class_body(input)?;
	Ok((
		input,
		Class {
			attributes,
			name: name.into(),
			class_keyword: class_specifier.into(),
			superclass: is_superclass.map(|is_soup| is_soup.3.into()),
			body,
		},
	))
}

pub fn class_body(input: Span) -> ParserResult<ClassBody> {
	let (input, open_bracket) = tag("{")(input)?;
	let (input, after_open) = whitespace0(input)?;
	let open_bracket = Token::new_w_after(open_bracket, after_open);
	let (input, methods) = many0(method)(input)?;
	let (input, before_end) = multispace0(input)?;
	let (input, close_bracket) = tag("}")(input)?;
	let (input, after_end) = whitespace0(input)?;
	let close_bracket = Token::new(before_end, close_bracket, after_end);

	Ok((input, ClassBody{opening_bracket: open_bracket, methods, closing_bracket: close_bracket}))
}

pub fn method(input: Span) -> ParserResult<Method> {
	let (input, attributes) = many0(attribute)(input)?;

	let (input, construct) = opt(tuple((multispace0, tag("construct"), multispace1)))(input)?;
	let construct = construct.map(|t|Token::new(t.0, t.1, t.2));

	let (input, static_keyword) = opt(tuple((multispace0, tag("static"), multispace1)))(input)?;
	let static_keyword = static_keyword.map(|t|Token::new(t.0, t.1, t.2));

	let (input, foreign) = opt(tuple((multispace0, tag("foreign"), multispace1)))(input)?; //order?
	let foreign = foreign.map(|t|Token::new(t.0, t.1, t.2));
	
	let (input, before_name) = multispace0(input)?;
	let (input, name) = alphanumeric1(input)?;
	let (input, after_name) = multispace0(input)?;
	let name = Token::new(before_name, name, after_name);

	let (input, setter_equals) = opt(tuple((tag("="), multispace0)))(input)?;
	let setter_equals = setter_equals.map(|t|Token::new_w_after(t.0, t.1));

	let (input, arguments) = opt(delimited(tag("("), separated_list0(tag(","), argument), tag(")")))(input)?;

	let method_type = if setter_equals.is_some() { MethodType::Setter } else if arguments.is_some() {MethodType::Method} else {MethodType::Getter};

	let (input, body) = method_body(input)?;

	Ok((input, Method{ 
		attributes, 
		construct,
		static_keyword, 
		foreign, 
		name, 
		setter_equals,
		arguments, 
		method_type, 
		body }))
}

pub fn method_body(input: Span) -> ParserResult<MethodBody> {
	let (input, before_body) = multispace0(input)?;
	let (input, open) = tag("{")(input)?;
	let open = Token::new_w_before(before_body, open);
	let (input, content) = take_until("}")(input)?;
	let (input, close) = tag("}")(input)?;
	let (input, after_body) = whitespace0(input)?;
	let close = Token::new_w_after(close, after_body);

	Ok((input, MethodBody{opening_bracket: open, logic: Vec::new(), closing_bracket: close}))
}

pub fn argument(input: Span) -> ParserResult<Argument> {
	let (input, before_name) = multispace0(input)?;
	let (input, name) = alphanumeric1(input)?;
	let (input, after_name) = multispace0(input)?;
	let name = Token::new(before_name, name, after_name);

	let (input, type_hint) = opt(tuple((char(':'), multispace0, alphanumeric1, multispace0)))(input)?;
	let type_hint = type_hint.map(|t| Token::new(t.1, t.2, t.3));

	Ok((input, Argument{name, type_hint}))
}

pub fn attribute(input: Span) -> ParserResult<Attribute> {
	let (input, before_tag) = multispace0(input)?;
	let (input, tag_char) = tag("#")(input)?;
	let (input, after_tag) = multispace0(input)?;
	let tag_char = Token::new(before_tag, tag_char, after_tag);
	let (input, runtime_specifier) = opt(tuple((tag("!"), multispace0)))(input)?;
	let runtime_specifier = runtime_specifier.map(|t| Token::new_w_after(t.0, t.1));
	let (input, key) = alphanumeric1(input)?;
	let (input, after_key) = multispace0(input)?;
	let key = Token::new_w_after(key, after_key);
	let (input, attribute) = attribute_value(input)?; //todo: parse subattributes

	Ok((
		input,
		Attribute {
			name: key,
			tag: tag_char,
			runtime_specifier,
			value: attribute,
		},
	))
}

pub fn attribute_value(input: Span) -> ParserResult<AttributeValue> {
	let (input, _) = char('=')(input)?;
	let (input, _) = multispace0(input)?;
	let (input, expr) = expr(input)?;

	Ok((input, expr.into()))
}

pub fn import_variable(input: Span) -> ParserResult<ImportVar> {
	let (input, var_tokens) = tuple((
		opt(tuple((alphanumeric1, multispace1, tag("as"), multispace1))),
		alphanumeric1,
	))(input)?;
	let name: Token = var_tokens.1.into();
	let source_as = var_tokens.0;
	let as_keyword = source_as.map(|as_kw| Token::new(as_kw.1, as_kw.2, as_kw.3));
	let source = source_as.map_or(name.clone(), |as_kw| as_kw.0.into());
	Ok((input, ImportVar { name, as_keyword, source }))
}

pub fn as_expr<'a, I, O, E: ParseError<I>, F>(first: F) -> impl FnMut(I) -> IResult<I, Expr<'a>, E>
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

pub fn as_stmt<'a, I, O, E: ParseError<I>, F>(first: F) -> impl FnMut(I) -> IResult<I, Stmt<'a>, E>
where
	F: Parser<I, O, E>,
	O: std::convert::Into<Stmt<'a>>,
{
	as_other::<_, _, Stmt, _, _>(first)
}

fn as_other<I, O, T, E: ParseError<I>, F>(mut first: F) -> impl FnMut(I) -> IResult<I, T, E>
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
	let (input, before) = multispace0(input)?;
	let (input, string) = quote_string(input)?;
	let (input, after) = whitespace0(input)?;
	let expr = StringExpr { string: Token::new(before, string, after) };
	Ok((input, expr))
}

//todo: support string interpolation and multiline strings
pub fn quote_string(input: Span) -> ParserResult<Span> {
	let original_input = input;
	let (input, start_quotes) = alt((tag("\"\"\""), tag("\"")))(input)?;
	let multiline = start_quotes.fragment().eq(&"\"\"\"");
	let (input, string) = if multiline {
		escaped(take_until("\"\"\""), '\\', one_of("\\\""))(input)?
	} else {
		escaped(is_not("\"\n"), '\\', one_of("\\\""))(input)?
	};
	let (input, end_quotes) = alt((tag("\"\"\""), tag("\"")))(input)?;
	let string = original_input.slice(..(string.len() + start_quotes.len() + end_quotes.len()));
	Ok((input, string))
}

//todo: parse comments as whitespace
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
