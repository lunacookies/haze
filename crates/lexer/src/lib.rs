use std::mem;

use logos::Logos;
use text_size::TextSize;
use token::Tokens;

pub fn lex(input: &str) -> Tokens {
	let mut kinds = Vec::new();
	let mut starts = Vec::new();

	assert_eq!(TokenKind::Last as u8, token::TokenKind::Last as u8);

	let lexer = TokenKind::lexer(input).spanned();
	for (kind, span) in lexer {
		let kind = kind.unwrap_or(TokenKind::Error);
		let kind = unsafe { mem::transmute::<TokenKind, token::TokenKind>(kind) };
		kinds.push(kind);
		starts.push(TextSize::from(span.start as u32));
	}

	starts.push(TextSize::from(input.len() as u32));

	Tokens::new(kinds, starts)
}

#[derive(Logos)]
#[repr(u8)]
enum TokenKind {
	#[regex("_?[a-zA-Z][a-zA-Z0-9_]*")]
	Identifier,

	#[regex("[ \n\t]+")]
	Whitespace,

	Error,

	Last,
}

#[cfg(test)]
mod tests {
	use std::{env, ffi::OsStr, fs, io};

	use super::*;

	#[test]
	fn run_test() -> io::Result<()> {
		const SEPARATOR: &str = "\n---\n";

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
			dbg!(&expected, &content);
			expect_test::expect_file![path].assert_eq(&expected);
		}

		Ok(())
	}
}
