#![warn(rust_2018_idioms, unreachable_pub)]

use std::path::PathBuf;

use text_size::TextRange;

#[derive(Debug, Default)]
pub struct Diagnostics {
	diagnostics: Vec<Diagnostic>,
}

#[derive(Debug)]
pub struct DiagnosticsContext<'a> {
	file: PathBuf,
	diagnostics: &'a mut Diagnostics,
}

#[derive(Debug)]
pub struct Diagnostic {
	pub message: String,
	pub range: Option<TextRange>,
	pub file: PathBuf,
}

impl Diagnostics {
	pub fn new() -> Diagnostics {
		Diagnostics::default()
	}

	pub fn add(&mut self, message: String, range: TextRange, file: PathBuf) {
		let diagnostic = Diagnostic { message, range: Some(range), file };
		self.diagnostics.push(diagnostic);
	}

	pub fn add_without_range(&mut self, message: String, file: PathBuf) {
		let diagnostic = Diagnostic { message, range: None, file };
		self.diagnostics.push(diagnostic);
	}

	pub fn context(&mut self, file: PathBuf) -> DiagnosticsContext<'_> {
		DiagnosticsContext { file, diagnostics: self }
	}

	pub fn diagnostics(&self) -> &[Diagnostic] {
		&self.diagnostics
	}
}

impl DiagnosticsContext<'_> {
	pub fn add(&mut self, message: String, range: TextRange) {
		self.diagnostics.add(message, range, self.file.clone())
	}

	pub fn add_without_range(&mut self, message: String) {
		self.diagnostics.add_without_range(message, self.file.clone());
	}
}
