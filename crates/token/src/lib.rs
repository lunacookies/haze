use std::fmt;

use text_size::{TextRange, TextSize};

pub struct Tokens {
	kinds: Vec<TokenKind>,
	starts: Vec<TextSize>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[repr(u8)]
pub enum TokenKind {
	Whitespace,
	Error,

	#[doc(hidden)]
	Last,
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

impl fmt::Debug for Tokens {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		for i in 0..self.kinds.len() {
			let kind = self.kind(i);
			let range = self.range(i);
			writeln!(f, "{kind:?}@{range:?}")?;
		}

		Ok(())
	}
}
