#![warn(rust_2018_idioms, unreachable_pub)]

use std::fmt;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[repr(u8)]
pub enum NodeKind {
	SourceFile,
	Function,
	ParameterList,
	Parameter,
	Type,
	BlockExpression,
	VariableDeclaration,
	ExpressionStatement,
	Call,
	Argument,
	ArgumentLabel,
	PathExpression,
	IfExpression,
	IntegerLiteral,
	Error,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[repr(u8)]
pub enum TokenKind {
	FuncKw,
	IfKw,
	ElseKw,
	Identifier,
	Underscore,
	Number,
	OpenParenthesis,
	CloseParenthesis,
	OpenBracket,
	CloseBracket,
	OpenBrace,
	CloseBrace,
	Less,
	Greater,
	Equals,
	Plus,
	Hyphen,
	Star,
	Slash,
	Percent,
	Bang,
	Tilde,
	Ampersand,
	Pipe,
	Caret,
	Hash,
	Dollar,
	Comma,
	Period,
	Colon,
	Semicolon,
	Question,
	At,
	Backslash,
	Backtick,
	Arrow,
	ColonEquals,

	Whitespace,
	Error,
	EndOfFile,

	#[doc(hidden)]
	Last,
}

impl TokenKind {
	pub fn is_trivia(self) -> bool {
		matches!(self, TokenKind::Whitespace)
	}
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum TreeConfig {}

unsafe impl eventree::TreeConfig for TreeConfig {
	type NodeKind = NodeKind;
	type TokenKind = TokenKind;

	fn node_kind_to_raw(node_kind: Self::NodeKind) -> u16 {
		node_kind as u16
	}

	fn token_kind_to_raw(token_kind: Self::TokenKind) -> u16 {
		token_kind as u16
	}

	unsafe fn node_kind_from_raw(raw: u16) -> Self::NodeKind {
		std::mem::transmute(raw as u8)
	}

	unsafe fn token_kind_from_raw(raw: u16) -> Self::TokenKind {
		std::mem::transmute(raw as u8)
	}
}

pub type SyntaxTree = eventree::SyntaxTree<TreeConfig>;
pub type SyntaxBuilder = eventree::SyntaxBuilder<TreeConfig>;

pub type SyntaxNode = eventree::SyntaxNode<TreeConfig>;
pub type SyntaxToken = eventree::SyntaxToken<TreeConfig>;
pub type SyntaxElement = eventree::SyntaxElement<TreeConfig>;

impl fmt::Display for TokenKind {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		let s = match self {
			TokenKind::FuncKw => "“func”",
			TokenKind::IfKw => "“if”",
			TokenKind::ElseKw => "“else”",
			TokenKind::Identifier => "identifier",
			TokenKind::Underscore => "“_”",
			TokenKind::Number => "number",
			TokenKind::OpenParenthesis => "“(”",
			TokenKind::CloseParenthesis => "“)”",
			TokenKind::OpenBracket => "“[”",
			TokenKind::CloseBracket => "“]”",
			TokenKind::OpenBrace => "“{”",
			TokenKind::CloseBrace => "“}”",
			TokenKind::Less => "“<”",
			TokenKind::Greater => "“>”",
			TokenKind::Equals => "“=”",
			TokenKind::Plus => "“+”",
			TokenKind::Hyphen => "“-”",
			TokenKind::Star => "“*”",
			TokenKind::Slash => "“/”",
			TokenKind::Percent => "“%”",
			TokenKind::Bang => "“!”",
			TokenKind::Tilde => "“~”",
			TokenKind::Ampersand => "“&”",
			TokenKind::Pipe => "“|”",
			TokenKind::Caret => "“^”",
			TokenKind::Hash => "“#”",
			TokenKind::Dollar => "“$”",
			TokenKind::Comma => "“,”",
			TokenKind::Period => "“.”",
			TokenKind::Colon => "“:”",
			TokenKind::Semicolon => "“;”",
			TokenKind::Question => "“?”",
			TokenKind::At => "“@”",
			TokenKind::Backslash => "“\\”",
			TokenKind::Backtick => "“`”",
			TokenKind::Arrow => "“->”",
			TokenKind::ColonEquals => "“:=”",

			TokenKind::Whitespace => "whitespace",
			TokenKind::Error => "bad token",
			TokenKind::EndOfFile => "end of file",

			TokenKind::Last => unreachable!(),
		};

		f.write_str(s)
	}
}
