#![warn(rust_2018_idioms, unreachable_pub)]

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[repr(u8)]
pub enum NodeKind {
	SourceFile,
	BinaryExpr,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[repr(u8)]
pub enum TokenKind {
	FuncKw,
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

	Whitespace,
	Error,

	#[doc(hidden)]
	Last,
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
