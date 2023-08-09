#![warn(rust_2018_idioms, unreachable_pub)]

mod grammar;

use diagnostics::DiagnosticsContext;
use syntax::{NodeKind, SyntaxBuilder, SyntaxTree};
use token::Tokens;

pub fn parse_source_file(
	text: &str,
	tokens: &Tokens,
	diagnostics: DiagnosticsContext<'_>,
) -> SyntaxTree {
	let mut parser = Parser::new(tokens, diagnostics);
	grammar::source_file(&mut parser);
	let events = parser.finish();
	process_events(text, tokens, &events)
}

struct Parser<'a> {
	tokens: &'a Tokens,
	cursor: usize,
	events: Vec<Event>,
	diagnostics: DiagnosticsContext<'a>,
}

#[derive(Debug, Clone, Copy)]
enum Event {
	StartNode(NodeKind),
	AddToken,
	FinishNode,
	Placeholder,
}

impl Parser<'_> {
	fn new<'a>(tokens: &'a Tokens, diagnostics: DiagnosticsContext<'a>) -> Parser<'a> {
		Parser { tokens, cursor: 0, events: Vec::new(), diagnostics }
	}

	fn start(&mut self) -> Marker {
		let position = self.events.len() as u32;
		self.events.push(Event::Placeholder);
		Marker { position }
	}

	fn finish(self) -> Vec<Event> {
		assert!(self.at_eof());
		self.events
	}

	fn at_eof(&self) -> bool {
		self.cursor == self.tokens.len()
	}
}

struct Marker {
	position: u32,
}

impl Marker {
	fn complete(self, p: &mut Parser<'_>, node_kind: NodeKind) -> CompletedMarker {
		p.events[self.position as usize] = Event::StartNode(node_kind);
		p.events.push(Event::FinishNode);
		CompletedMarker { position: self.position }
	}
}

struct CompletedMarker {
	position: u32,
}

fn process_events(text: &str, tokens: &Tokens, events: &[Event]) -> SyntaxTree {
	let mut builder = SyntaxBuilder::new(text);
	let mut cursor = 0;

	for &event in events {
		match event {
			Event::StartNode(node_kind) => builder.start_node(node_kind),

			Event::AddToken => {
				let kind = tokens.kind(cursor);
				let range = tokens.range(cursor);
				builder.add_token(kind, range);
				cursor += 1;
			}

			Event::FinishNode => builder.finish_node(),

			Event::Placeholder => unreachable!(),
		}
	}

	builder.finish()
}

#[cfg(test)]
mod tests {
	use std::{env, ffi::OsStr, fs, io, path::PathBuf};

	use super::*;

	#[test]
	fn run_test() -> io::Result<()> {
		const SEPARATOR: &str = "\n======\n";

		let tests_dir = env::current_dir()?.join("test-data");
		for entry in fs::read_dir(tests_dir)? {
			let entry = entry?;

			let file_type = entry.file_type()?;
			if !file_type.is_file() {
				panic!("found non-file in parser/test-data!");
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

			let mut diagnostics = diagnostics::Diagnostics::new();
			let diagnostics_context = diagnostics.context(PathBuf::from("test"));
			let tokens = lexer::lex(input);
			let syntax = parse_source_file(input, &tokens, diagnostics_context);

			let mut expected = format!("{input}{SEPARATOR}{syntax:#?}");
			for diagnostic in diagnostics.diagnostics() {
				expected.push_str(&format!(
					"{}:{:?}: {}",
					diagnostic.file.display(),
					diagnostic.range,
					diagnostic.message
				));
			}

			expect_test::expect_file![path].assert_eq(&expected);
		}

		Ok(())
	}
}
