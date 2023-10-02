use std::path::PathBuf;

use crate::lexer::{lex, Token, TokenKind};

pub fn parse(input: &str, file: PathBuf) -> Ast {
	let tokens = lex(input, file);
	Parser::new(tokens).parse()
}

pub struct Ast {
	definitions: Vec<Definition>,
}

pub enum Definition {
	Procedure(Procedure),
}

pub struct Procedure {
	pub name: String,
	pub parameters: Vec<Parameter>,
}

pub struct Parameter {
	name: String,
	ty: Ty,
}

pub enum Ty {
	Int,
}

struct Parser {
	tokens: Vec<Token>,
	cursor: usize,
}

impl Parser {
	fn new(tokens: Vec<Token>) -> Parser {
		Parser { tokens, cursor: 0 }
	}

	fn parse(mut self) -> Ast {
		let mut definitions = Vec::new();

		while !self.at_end() {
			definitions.push(self.parse_definition());
		}

		Ast { definitions }
	}

	fn parse_definition(&mut self) -> Definition {
		match self.current() {
			TokenKind::ProcKw => self.parse_procedure(),
			_ => self.error("expected definition".to_string()),
		}
	}

	fn parse_procedure(&mut self) -> Definition {
		self.bump(TokenKind::ProcKw);
		let name = self.bump_text(TokenKind::Identifier);
		self.bump(TokenKind::LParen);
		self.bump(TokenKind::RParen);
		self.bump(TokenKind::LBrace);
		self.bump(TokenKind::RBrace);

		Definition::Procedure(Procedure { name, parameters: Vec::new() })
	}

	fn bump_text(&mut self, kind: TokenKind) -> String {
		let text = self.tokens[self.cursor].text.clone();
		self.bump(kind);
		text
	}

	fn bump(&mut self, kind: TokenKind) {
		assert!(self.at(kind));
		self.cursor += 1;
	}

	fn at(&self, kind: TokenKind) -> bool {
		self.current() == kind
	}

	fn current(&self) -> TokenKind {
		self.tokens[self.cursor].kind
	}

	fn at_end(&self) -> bool {
		self.cursor >= self.tokens.len()
	}

	fn error(&self, msg: String) -> ! {
		crate::error(self.tokens[self.cursor].loc.clone(), msg);
	}
}

impl Ast {
	pub fn pretty_print(&self) -> String {
		let mut ctx = PrettyPrintCtx { buf: String::new() };
		ctx.print_ast(self);
		ctx.buf
	}
}

struct PrettyPrintCtx {
	buf: String,
}

impl PrettyPrintCtx {
	fn print_ast(&mut self, ast: &Ast) {
		for definition in &ast.definitions {
			match definition {
				Definition::Procedure(proc) => self.print_procedure(proc),
			}
		}
	}

	fn print_procedure(&mut self, proc: &Procedure) {
		self.s("proc ");
		self.s(&proc.name);
		self.s("(");

		for (i, parameter) in proc.parameters.iter().enumerate() {
			if i != 0 {
				self.s(", ");
			}

			self.s(&parameter.name);
			self.s(" ");
			self.print_ty(&parameter.ty);
		}

		self.s(")\n");
	}

	fn print_ty(&mut self, ty: &Ty) {
		match ty {
			Ty::Int => self.s("int"),
		}
	}

	fn s(&mut self, s: &str) {
		self.buf.push_str(s);
	}
}

#[cfg(test)]
#[test]
fn tests() {
	crate::testing::run_tests("test_data_parser", |input| {
		let ast = parse(input, PathBuf::from("test"));
		ast.pretty_print()
	});
}
