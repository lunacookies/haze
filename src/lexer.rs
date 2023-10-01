use std::{fmt, path::PathBuf};

pub fn lex(text: &str, file: PathBuf) -> Vec<Token> {
	let bytes = text.as_bytes();
	let mut tokens = Vec::new();
	let mut i = 0;
	let mut line = 1;
	let mut column = 1;

	while i < bytes.len() {
		if bytes[i] == b' ' {
			i += 1;
			column += 1;
			continue;
		}

		if bytes[i] == b'\n' {
			i += 1;
			line += 1;
			column = 1;
			continue;
		}

		if bytes[i].is_ascii_alphabetic() || bytes[i] == b'_' {
			let start = i;
			let loc = Loc { line, column, file: file.clone() };

			while bytes[i].is_ascii_alphanumeric() || bytes[i] == b'_' {
				i += 1;
				column += 1;
			}

			tokens.push(Token {
				kind: TokenKind::Identifier,
				text: text[start..i].to_string(),
				loc,
			});
		}
	}

	tokens
}

pub struct Token {
	pub kind: TokenKind,
	pub text: String,
	pub loc: Loc,
}

#[derive(Debug)]
pub enum TokenKind {
	Identifier,
}

pub struct Loc {
	pub line: u32,
	pub column: u32,
	pub file: PathBuf,
}

impl fmt::Debug for Loc {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		write!(f, "{}:{}:{}", self.file.display(), self.line, self.column)
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
