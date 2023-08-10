#![warn(rust_2018_idioms, unreachable_pub)]

use std::mem;

use logos::Logos;
use text_size::TextSize;
use token::Tokens;

pub fn lex(input: &str) -> Tokens {
	let mut kinds = Vec::new();
	let mut starts = Vec::new();

	assert_eq!(TokenKind::Last as u8, syntax::TokenKind::Last as u8);

	let lexer = TokenKind::lexer(input).spanned();
	for (kind, span) in lexer {
		let kind = kind.unwrap_or(TokenKind::Error);
		let kind = unsafe { mem::transmute::<TokenKind, syntax::TokenKind>(kind) };
		kinds.push(kind);
		starts.push(TextSize::from(span.start as u32));
	}

	starts.push(TextSize::from(input.len() as u32));

	Tokens::new(kinds, starts)
}

#[derive(Logos)]
#[repr(u8)]
#[allow(dead_code)]
enum TokenKind {
	#[token("func")]
	FuncKw,

	#[regex("_?[a-zA-Z][a-zA-Z0-9_]*")]
	Identifier,
	#[token("_")]
	Underscore,

	#[regex("[0-9]+")]
	Number,

	#[token("(")]
	OpenParenthesis,
	#[token(")")]
	CloseParenthesis,
	#[token("[")]
	OpenBracket,
	#[token("]")]
	CloseBracket,
	#[token("{")]
	OpenBrace,
	#[token("}")]
	CloseBrace,
	#[token("<")]
	Less,
	#[token(">")]
	Greater,

	#[token("=")]
	Equals,

	#[token("+")]
	Plus,
	#[token("-")]
	Hyphen,
	#[token("*")]
	Star,
	#[token("/")]
	Slash,
	#[token("%")]
	Percent,

	#[token("!")]
	Bang,
	#[token("~")]
	Tilde,

	#[token("&")]
	Ampersand,
	#[token("|")]
	Pipe,
	#[token("^")]
	Caret,

	#[token("#")]
	Hash,

	#[token("$")]
	Dollar,

	#[token(",")]
	Comma,

	#[token(".")]
	Period,

	#[token(":")]
	Colon,

	#[token(";")]
	Semicolon,

	#[token("?")]
	Question,

	#[token("@")]
	At,

	#[token("\\")]
	Backslash,

	#[token("`")]
	Backtick,

	Arrow,
	ColonEquals,

	#[regex("[ \n\t]+")]
	Whitespace,
	Error,
	EndOfFile,

	Last,
}

#[cfg(test)]
mod tests {
	use std::{env, ffi::OsStr, fs, io};

	use super::*;

	#[test]
	fn run_test() -> io::Result<()> {
		const SEPARATOR: &str = "\n======\n";

		let tests_dir = env::current_dir()?.join("test-data");
		for entry in fs::read_dir(tests_dir)? {
			let entry = entry?;

			let file_type = entry.file_type()?;
			if !file_type.is_file() {
				panic!("found non-file in lexer/test-data!");
			}

			let path = entry.path();

			if path.extension() != Some(OsStr::new("test")) {
				panic!("test at {} doesn’t have .test extension", path.display());
			}

			// This unwrap can never fire because we must have a file now,
			// not a directory.
			let test_name = path.file_stem().unwrap();

			let content = fs::read_to_string(&path)?;
			let Some((input, _actual)) = content.split_once(SEPARATOR) else {
                panic!("no separator in test {}", test_name.to_string_lossy());
            };

			let tokens = lex(input);
			let expected = format!("{input}{SEPARATOR}{}", tokens.debug(input));
			expect_test::expect_file![path].assert_eq(&expected);
		}

		Ok(())
	}
}
