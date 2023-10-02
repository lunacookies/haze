use std::{cell::Cell, path::PathBuf};

use crate::lexer::{lex, Token, TokenKind};

pub fn parse(input: &str, file: PathBuf) -> Ast {
	let tokens = lex(input, file);
	Parser::new(tokens).parse()
}

#[derive(Clone, PartialEq, Eq)]
pub struct Ast {
	pub definitions: Vec<Definition>,
}

#[derive(Clone, PartialEq, Eq)]
pub enum Definition {
	Procedure(Procedure),
}

#[derive(Clone, PartialEq, Eq)]
pub struct Procedure {
	pub name: String,
	pub parameters: Vec<Parameter>,
	pub return_ty: Option<Ty>,
	pub body: Statement,
}

#[derive(Clone, PartialEq, Eq)]
pub struct Parameter {
	pub name: String,
	pub ty: Ty,
}

#[derive(Clone, PartialEq, Eq)]
pub enum Statement {
	Expression(Expression),
	Block(Vec<Statement>),
}

#[derive(Clone, PartialEq, Eq)]
pub enum Expression {
	Integer(u64),
}

#[derive(Clone, PartialEq, Eq)]
pub enum Ty {
	Int,
}

struct Parser {
	tokens: Vec<Token>,
	cursor: usize,
	fuel: Cell<u8>,
}

const INITIAL_FUEL: u8 = 255;

impl Parser {
	fn new(tokens: Vec<Token>) -> Parser {
		Parser { tokens, cursor: 0, fuel: Cell::new(INITIAL_FUEL) }
	}

	fn parse(mut self) -> Ast {
		let mut definitions = Vec::new();

		while !self.at_eof() {
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
		let name = self.expect_text(TokenKind::Identifier);

		self.expect(TokenKind::LParen);
		let mut parameters = Vec::new();

		while !self.at_eof() && !self.at(TokenKind::RParen) {
			let parameter_name = self.expect_text(TokenKind::Identifier);
			let parameter_ty = self.parse_ty();
			parameters.push(Parameter { name: parameter_name, ty: parameter_ty });

			match (self.current(), self.lookahead()) {
				(TokenKind::RParen, _) => {}
				(TokenKind::Comma, TokenKind::RParen) => self.bump(TokenKind::Comma),
				_ => self.expect(TokenKind::Comma),
			}

			if self.at(TokenKind::RParen) {
				self.eat(TokenKind::Comma);
			}
		}

		self.expect(TokenKind::RParen);

		let mut return_ty = None;
		if !self.at(TokenKind::LBrace) {
			return_ty = Some(self.parse_ty());
		}

		let body = self.parse_block();

		Definition::Procedure(Procedure { name, parameters, return_ty, body })
	}

	fn parse_statement(&mut self) -> Statement {
		match self.current() {
			TokenKind::LBrace => self.parse_block(),
			_ => Statement::Expression(self.parse_expression()),
		}
	}

	fn parse_block(&mut self) -> Statement {
		self.bump(TokenKind::LBrace);

		let mut statements = Vec::new();
		while !self.at_eof() && !self.at(TokenKind::RBrace) {
			statements.push(self.parse_statement());
		}

		self.expect(TokenKind::RBrace);

		Statement::Block(statements)
	}

	fn parse_expression(&mut self) -> Expression {
		match self.current() {
			TokenKind::Integer => {
				let text = self.expect_text(TokenKind::Integer);
				Expression::Integer(text.parse().unwrap())
			}

			_ => self.error("expected expression".to_string()),
		}
	}

	fn parse_ty(&mut self) -> Ty {
		let text = self.expect_text(TokenKind::Identifier);
		match text.as_str() {
			"int" => Ty::Int,
			_ => self.error("expected type".to_string()),
		}
	}

	fn expect_text(&mut self, kind: TokenKind) -> String {
		let text = self.tokens[self.cursor].text.clone();
		self.expect(kind);
		text
	}

	fn expect(&mut self, kind: TokenKind) {
		if !self.eat(kind) {
			self.error(format!("expected {kind:?} but found {:?}", self.current()));
		}
	}

	fn eat(&mut self, kind: TokenKind) -> bool {
		if self.at(kind) {
			self.bump(kind);
			return true;
		}
		false
	}

	fn bump(&mut self, kind: TokenKind) {
		assert!(self.at(kind));
		self.cursor += 1;
		self.fuel.set(INITIAL_FUEL);
	}

	fn at(&self, kind: TokenKind) -> bool {
		self.current() == kind
	}

	fn current(&self) -> TokenKind {
		let remaining_fuel = self.fuel.get();
		if remaining_fuel == 0 {
			panic!("parser ran out of fuel");
		}
		self.fuel.set(remaining_fuel - 1);

		if self.at_eof() {
			return TokenKind::Eof;
		}
		self.tokens[self.cursor].kind
	}

	fn lookahead(&self) -> TokenKind {
		if self.cursor + 1 >= self.tokens.len() {
			return TokenKind::Eof;
		}
		self.tokens[self.cursor + 1].kind
	}

	fn at_eof(&self) -> bool {
		self.cursor >= self.tokens.len()
	}

	fn error(&self, msg: String) -> ! {
		let loc = if self.at_eof() {
			self.tokens[self.tokens.len() - 1].loc.clone()
		} else {
			self.tokens[self.cursor].loc.clone()
		};

		crate::error(loc, msg);
	}
}

impl Ast {
	pub fn pretty_print(&self) -> String {
		let mut ctx = PrettyPrintCtx { buf: String::new(), indentation: 0 };
		ctx.print_ast(self);
		ctx.buf
	}
}

struct PrettyPrintCtx {
	buf: String,
	indentation: usize,
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

		self.s(") ");

		if let Some(return_ty) = &proc.return_ty {
			self.print_ty(return_ty);
			self.s(" ");
		}

		if proc.body == Statement::Block(Vec::new()) {
			self.s("{}");
		} else {
			self.newline();
			self.print_statement(&proc.body);
		}

		self.s("\n");
	}

	fn print_statement(&mut self, statement: &Statement) {
		match statement {
			Statement::Expression(e) => self.print_expression(e),
			Statement::Block(statements) => {
				self.s("{");
				self.indentation += 1;

				for statement in statements {
					self.newline();
					self.print_statement(statement);
				}

				self.indentation -= 1;
				self.newline();
				self.s("}");
			}
		}
	}

	fn print_expression(&mut self, expression: &Expression) {
		match expression {
			Expression::Integer(i) => self.s(&format!("{i}")),
		}
	}

	fn print_ty(&mut self, ty: &Ty) {
		match ty {
			Ty::Int => self.s("int"),
		}
	}

	fn s(&mut self, s: &str) {
		self.buf.push_str(s);
	}

	fn newline(&mut self) {
		self.buf.push('\n');
		for _ in 0..self.indentation {
			self.buf.push('\t');
		}
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
