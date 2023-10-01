use std::path::PathBuf;

pub fn lex(text: &str, file: PathBuf) -> Vec<Token> {
	Vec::new()
}

pub struct Token {
	pub kind: TokenKind,
	pub text: String,
	pub loc: Loc,
}

#[derive(Debug)]
pub enum TokenKind {}

pub struct Loc {
	pub line: u32,
	pub column: u32,
	pub file: PathBuf,
}

#[cfg(test)]
#[test]
fn tests() {
	use std::fmt::Write;

	crate::testing::run_tests("test_data_lexer", |input| {
		let tokens = lex(input, PathBuf::from("test"));
		let mut s = String::new();

		for token in tokens {
			writeln!(
				s,
				"{:?}@{}:{} {:?}",
				token.kind, token.loc.line, token.loc.column, token.text
			)
			.unwrap();
		}

		s
	});
}
