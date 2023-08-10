#![warn(rust_2018_idioms, unreachable_pub)]

mod grammar;

use diagnostics::DiagnosticsContext;
use syntax::{NodeKind, SyntaxBuilder, SyntaxTree, TokenKind};
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

	fn finish(mut self) -> Vec<Event> {
		assert!(self.at_eof());
		self.events
	}

	fn advance_with_error(&mut self, message: &str) -> Option<CompletedMarker> {
		if self.at_eof() {
			let range = self.tokens.range(self.tokens.len() - 1);
			self.diagnostics.add(message.to_string(), range);
			return None;
		}

		let range = self.tokens.range(self.cursor);
		self.diagnostics.add(message.to_string(), range);
		let m = self.start();
		self.bump_any();
		Some(m.complete(self, NodeKind::Error))
	}

	fn expect(&mut self, kind: TokenKind) {
		self.expect_with_name(kind, &format!("expected {kind}"));
	}

	fn expect_with_name(&mut self, kind: TokenKind, name: &str) {
		if self.eat(kind) {
			return;
		}

		self.advance_with_error(name);
	}

	fn bump(&mut self, kind: TokenKind) {
		assert!(self.eat(kind));
	}

	fn eat(&mut self, kind: TokenKind) -> bool {
		if !self.at(kind) {
			return false;
		}

		let token_count = match kind {
			TokenKind::Arrow => 2,
			TokenKind::ColonEquals => 2,
			_ => 1,
		};

		for _ in 0..token_count {
			self.bump_any();
		}

		true
	}

	fn bump_any(&mut self) {
		assert!(!self.at_eof());
		self.events.push(Event::AddToken);
		self.cursor += 1;
	}

	fn at(&mut self, kind: TokenKind) -> bool {
		self.skip_trivia();

		let (first, second) = match kind {
			TokenKind::Arrow => (TokenKind::Hyphen, TokenKind::Greater),
			TokenKind::ColonEquals => (TokenKind::Colon, TokenKind::Equals),
			_ => return self.current() == kind,
		};

		self.nth(0) == first && self.nth(1) == second
	}

	fn current(&mut self) -> TokenKind {
		self.nth(0)
	}

	fn nth(&mut self, n: usize) -> TokenKind {
		assert!(n <= 1);
		self.skip_trivia();

		if self.at_eof_raw() {
			return TokenKind::EndOfFile;
		}

		self.tokens.kind(self.cursor + n)
	}

	fn at_eof(&mut self) -> bool {
		self.skip_trivia();
		self.at_eof_raw()
	}

	fn at_eof_raw(&self) -> bool {
		self.cursor >= self.tokens.len()
	}

	fn skip_trivia(&mut self) {
		while !self.at_eof_raw() && self.tokens.kind(self.cursor).is_trivia() {
			self.cursor += 1;
		}
	}

	fn error(&mut self, message: &str) {
		let range = self.tokens.range(self.cursor);
		self.diagnostics.add(message.to_string(), range);
	}
}

struct Marker {
	position: u32,
}

impl Marker {
	fn complete(self, p: &mut Parser<'_>, kind: NodeKind) -> CompletedMarker {
		p.events[self.position as usize] = Event::StartNode(kind);
		p.events.push(Event::FinishNode);
		CompletedMarker
	}
}

struct CompletedMarker;

fn process_events(text: &str, tokens: &Tokens, events: &[Event]) -> SyntaxTree {
	let mut event_processor = EventProcessor::new(text, tokens, events);
	event_processor.process_events();
	event_processor.finish()
}

struct EventProcessor<'a> {
	events: &'a [Event],
	tokens: &'a Tokens,
	cursor: usize,
	builder: SyntaxBuilder,
}

impl EventProcessor<'_> {
	fn new<'a>(text: &str, tokens: &'a Tokens, events: &'a [Event]) -> EventProcessor<'a> {
		EventProcessor { events, tokens, cursor: 0, builder: SyntaxBuilder::new(text) }
	}

	fn process_events(&mut self) {
		match self.events[0] {
			Event::StartNode(kind) => self.builder.start_node(kind),
			Event::AddToken | Event::FinishNode | Event::Placeholder => {
				panic!("first event must be StartNode")
			}
		}

		for &event in &self.events[1..self.events.len() - 1] {
			self.skip_trivia();

			match event {
				Event::StartNode(kind) => self.builder.start_node(kind),
				Event::AddToken => self.add_token(),
				Event::FinishNode => self.builder.finish_node(),
				Event::Placeholder => unreachable!(),
			}
		}

		self.skip_trivia();

		let last_event = self.events[self.events.len() - 1];
		assert!(matches!(last_event, Event::FinishNode));
		self.builder.finish_node();
	}

	fn finish(self) -> SyntaxTree {
		self.builder.finish()
	}

	fn skip_trivia(&mut self) {
		while !self.at_eof() && self.tokens.kind(self.cursor).is_trivia() {
			self.add_token();
		}
	}

	fn at_eof(&mut self) -> bool {
		self.cursor >= self.tokens.len()
	}

	fn add_token(&mut self) {
		let kind = self.tokens.kind(self.cursor);
		let range = self.tokens.range(self.cursor);
		self.builder.add_token(kind, range);
		self.cursor += 1;
	}
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
				expected.push_str(&format!("{diagnostic}\n"));
			}

			expect_test::expect_file![path].assert_eq(&expected);
		}

		Ok(())
	}
}
