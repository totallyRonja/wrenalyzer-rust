use std::fmt::Debug;

use derive_more::From;
use nom_locate::LocatedSpan;
use flatdebug::FlatDebug;

pub type Span<'a> = LocatedSpan<&'a str>;



#[derive(Debug, Clone)]
pub struct ModuleElement<'a> {
	pub actions: Action<'a>,
	pub classes: Class<'a>
}

#[derive(Clone, From, FlatDebug)]
pub enum Action<'a> {
	ImportStmt(ImportStmt<'a>),
	StringExpr(StringExpr<'a>)
}

#[derive(Debug, Clone, From)]
pub enum Stmt<'a> {
	ImportStmt(ImportStmt<'a>),
}

#[derive(FlatDebug, Clone, From)]
pub enum Expr<'a> {
	StringExpr(StringExpr<'a>),
}

#[derive(Debug, Clone)]
pub struct Module<'a> {
	pub actions: Vec<Action<'a>>,
	pub imports: Vec<ImportStmt<'a>>,
	pub classes: Vec<Class<'a>>,
}

#[derive(Debug, Clone)]
pub struct Class<'a> {
	pub attributes: Vec<Attribute<'a>>,
	pub class_keyword: Token<'a>,
	pub name: Token<'a>,
	pub superclass: Option<Token<'a>>,
	pub methods: Vec<Method<'a>>
}

#[derive(Debug, Clone)]
pub struct Method<'a> {
	pub attributes: Vec<Attribute<'a>>,
	pub construct: Option<Token<'a>>,
	pub static_keyword: Option<Token<'a>>,
	pub foreign: Option<Token<'a>>,
	pub name: Token<'a>,
	pub logic: Vec<Action<'a>>
}

#[derive(Clone)]
pub struct ImportStmt<'a> {
	pub import_token: Token<'a>,
	pub path: Token<'a>,
	pub for_token: Token<'a>,
	pub variables: Vec<ImportVar<'a>>,
}

impl std::fmt::Debug for ImportStmt<'_> {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
			write!(f, "{}", self.import_token.text())?;
			write!(f, "{}", self.path.text())?;
			write!(f, "{}", self.for_token.text())?;
			f.debug_list().entries(self.variables.iter()).finish()
	}
}

#[derive(Clone)]
pub struct StringExpr<'a> {
	pub string: Token<'a>,
}

impl std::fmt::Debug for StringExpr<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "\"{}\"", self.string.text())
    }
}

#[derive(Debug, Clone)]
pub struct Attribute<'a>{
	pub tag:Token<'a>,
	pub runtime_specifier:Option<Token<'a>>,
	pub name:Token<'a>,
	pub value:AttributeValue<'a>
}

#[derive(Debug, Clone)]
pub enum AttributeValue<'a>{
	Expr(Expr<'a>),
	AttributeGroup(Vec<SubAttribute<'a>>),
}

#[derive(Debug, Clone)]
pub struct SubAttribute<'a>{
	pub name: Token<'a>,
	pub value: Expr<'a>
}

#[derive(Clone)]
pub struct ImportVar<'a> {
	pub name: Token<'a>,
	pub as_keyword: Option<Token<'a>>,
	pub source: Token<'a>,
}

impl std::fmt::Debug for ImportVar<'_> {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
			write!(f, "{}", self.source.text())?;
			if let Some(as_kw) = &self.as_keyword {
				write!(f, "{}", as_kw.text())?;
				write!(f, "{}", self.name.text())?;
			}
			Ok(())
	}
}

#[derive(Clone)]
pub struct Token<'a> {
	pub before: Option<&'a str>,
	pub core: Span<'a>,
	pub after: Option<&'a str>,
}

impl<'a> Token<'a>{
	pub fn new_core(span: Span<'a>) -> Self{
		Token{core: span, before: None, after: None}
	}

	pub fn new_w_before(before: Span<'a>, span: Span<'a>) -> Self{
		Token{before: Some(before.fragment()), core: span, after: None}
	}

	pub fn new_w_after(span: Span<'a>, after: Span<'a>) -> Self{
		Token{before: None, core: span, after: Some(after.fragment())}
	}

	pub fn new(before: Span<'a>, span: Span<'a>, after: Span<'a>) -> Self{
		Token{core: span, before: Some(before.fragment()), after: Some(after.fragment())}
	}

	pub fn text(&self) -> String {
		let mut text = String::new();
		if let Some(before) = self.before {
			text.push_str(before);
		}
		text.push_str(self.core.fragment());
		if let Some(after) = self.after {
			text.push_str(after);
		}
		text
	}
}

impl Debug for Token<'_> {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		write!(f, "{}", self.text())
	}
}

impl<'a> From<Span<'a>> for Token<'a> {
	fn from(span: Span<'a>) -> Self {
		Token::new_core(span)
	}
}