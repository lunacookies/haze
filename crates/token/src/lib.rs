#![warn(rust_2018_idioms, unreachable_pub)]

use std::fmt::Write;

use syntax::TokenKind;
use text_size::{TextRange, TextSize};

pub struct Tokens {
	kinds: Vec<TokenKind>,
	starts: Vec<TextSize>,
}

impl Tokens {
	pub fn new(kinds: Vec<TokenKind>, starts: Vec<TextSize>) -> Tokens {
		assert_eq!(kinds.len(), starts.len() - 1);
		Tokens { kinds, starts }
	}

	pub fn kind(&self, index: usize) -> TokenKind {
		self.kinds[index]
	}

	pub fn range(&self, index: usize) -> TextRange {
		TextRange::new(self.starts[index], self.starts[index + 1])
	}
}

impl Tokens {
	pub fn debug(&self, input: &str) -> String {
		let mut output = String::new();

		for i in 0..self.kinds.len() {
			let kind = self.kind(i);
			let range = self.range(i);
			writeln!(output, "{kind:?}@{range:?} {:?}", &input[range]).unwrap();
		}

		output
	}
}
