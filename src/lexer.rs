use std::{fmt, path::PathBuf};

pub fn lex(text: &str, file: PathBuf) -> Vec<Token> {
	Lexer::new(text, file).lex()
}

pub struct Token {
	pub kind: TokenKind,
	pub text: String,
	pub loc: Loc,
}

#[derive(Debug, Clone, Copy)]
pub enum TokenKind {
	Identifier,
	BangEqual,
	Bang,
	Hash,
	Dollar,
	PercentEqual,
	Percent,
	AndAndEqual,
	AndAnd,
	AndEqual,
	And,
	LParen,
	RParen,
	StarEqual,
	Star,
	PlusPlus,
	PlusEqual,
	Plus,
	Comma,
	MinusMinus,
	MinusEqual,
	Minus,
	DotDot,
	Dot,
	SlashEqual,
	Slash,
	Colon,
	Semi,
	LessLessEqual,
	LessLess,
	LessEqual,
	Less,
	EqualEqual,
	Equal,
	GreaterGreaterEqual,
	GreaterGreater,
	GreaterEqual,
	Greater,
	Question,
	At,
	LBracket,
	Backslash,
	RBracket,
	CaretEqual,
	Caret,
	Backtick,
	LBrace,
	PipePipeEqual,
	PipePipe,
	PipeEqual,
	Pipe,
	RBRace,
	TildeEqual,
	Tilde,
}

#[derive(Clone)]
pub struct Loc {
	pub line: usize,
	pub column: usize,
	pub file: PathBuf,
}

impl fmt::Debug for Loc {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		write!(f, "{}:{}:{}", self.file.display(), self.line, self.column)
	}
}

struct Lexer<'a> {
	text: &'a str,
	bytes: &'a [u8],
	i: usize,
	loc: Loc,
	tokens: Vec<Token>,
}

impl Lexer<'_> {
	fn new(text: &str, file: PathBuf) -> Lexer<'_> {
		Lexer {
			text,
			bytes: text.as_bytes(),
			i: 0,
			loc: Loc { line: 1, column: 1, file },
			tokens: Vec::new(),
		}
	}

	fn lex(mut self) -> Vec<Token> {
		while self.i < self.bytes.len() {
			self.step();
		}

		self.tokens
	}

	fn step(&mut self) {
		let loc = self.loc.clone();
		let start = self.i;

		match self.bytes[self.i] {
			b' ' => {
				self.i += 1;
				self.loc.column += 1;
				return;
			}

			b'\n' => {
				self.i += 1;
				self.loc.line += 1;
				self.loc.column = 1;
				return;
			}

			b'a'..=b'z' | b'A'..=b'Z' | b'_' => {
				while self.bytes[self.i].is_ascii_alphanumeric() || self.bytes[self.i] == b'_' {
					self.i += 1;
					self.loc.column += 1;
				}

				self.tokens.push(Token {
					kind: TokenKind::Identifier,
					text: self.text[start..self.i].to_string(),
					loc,
				});
				return;
			}

			_ => {}
		}

		let did_recognize = self.recognize(&[
			(b"!=", TokenKind::BangEqual),
			(b"!", TokenKind::Bang),
			(b"#", TokenKind::Hash),
			(b"$", TokenKind::Dollar),
			(b"%=", TokenKind::PercentEqual),
			(b"%", TokenKind::Percent),
			(b"&&=", TokenKind::AndAndEqual),
			(b"&&", TokenKind::AndAnd),
			(b"&=", TokenKind::AndEqual),
			(b"&", TokenKind::And),
			(b"(", TokenKind::LParen),
			(b")", TokenKind::RParen),
			(b"*=", TokenKind::StarEqual),
			(b"*", TokenKind::Star),
			(b"++", TokenKind::PlusPlus),
			(b"+=", TokenKind::PlusEqual),
			(b"+", TokenKind::Plus),
			(b",", TokenKind::Comma),
			(b"--", TokenKind::MinusMinus),
			(b"-=", TokenKind::MinusEqual),
			(b"-", TokenKind::Minus),
			(b"..", TokenKind::DotDot),
			(b".", TokenKind::Dot),
			(b"/=", TokenKind::SlashEqual),
			(b"/", TokenKind::Slash),
			(b":", TokenKind::Colon),
			(b";", TokenKind::Semi),
			(b"<<=", TokenKind::LessLessEqual),
			(b"<<", TokenKind::LessLess),
			(b"<=", TokenKind::LessEqual),
			(b"<", TokenKind::Less),
			(b"==", TokenKind::EqualEqual),
			(b"=", TokenKind::Equal),
			(b">>=", TokenKind::GreaterGreaterEqual),
			(b">>", TokenKind::GreaterGreater),
			(b">=", TokenKind::GreaterEqual),
			(b">", TokenKind::Greater),
			(b"?", TokenKind::Question),
			(b"@", TokenKind::At),
			(b"[", TokenKind::LBracket),
			(b"\\", TokenKind::Backslash),
			(b"]", TokenKind::RBracket),
			(b"^=", TokenKind::CaretEqual),
			(b"^", TokenKind::Caret),
			(b"`", TokenKind::Backtick),
			(b"{", TokenKind::LBrace),
			(b"||=", TokenKind::PipePipeEqual),
			(b"||", TokenKind::PipePipe),
			(b"|=", TokenKind::PipeEqual),
			(b"|", TokenKind::Pipe),
			(b"}", TokenKind::RBRace),
			(b"~=", TokenKind::TildeEqual),
			(b"~", TokenKind::Tilde),
		]);

		if !did_recognize {
			crate::error(self.loc.clone(), "invalid token".to_string());
		}
	}

	fn recognize(&mut self, options: &[(&[u8], TokenKind)]) -> bool {
		let loc = self.loc.clone();
		let start = self.i;

		for (prefix, kind) in options {
			if self.bytes[self.i..].starts_with(prefix) {
				self.i += prefix.len();
				self.loc.column += prefix.len();

				self.tokens.push(Token {
					kind: *kind,
					text: self.text[start..self.i].to_string(),
					loc,
				});

				return true;
			}
		}

		false
	}
}

#[cfg(test)]
#[test]
fn tests() {
	use fmt::Write;

	crate::testing::run_tests("test_data_lexer", |input| {
		let tokens = lex(input, PathBuf::from("test"));
		let mut s = String::new();

		for token in tokens {
			writeln!(s, "{:?} {:?} {:?}", token.kind, token.loc, token.text).unwrap();
		}

		s
	});
}
