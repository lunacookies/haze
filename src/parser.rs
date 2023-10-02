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
	LocalDeclaration { name: String, ty: Ty },
	LocalDefinition { name: String, value: Expression },
	Block(Vec<Statement>),
	Expression(Expression),
	Assignment { lhs: Expression, rhs: Expression },
}

#[derive(Clone, PartialEq, Eq)]
pub enum Expression {
	Integer(u64),
	Variable(String),
	True,
	False,
	Binary { lhs: Box<Expression>, operator: BinaryOperator, rhs: Box<Expression> },
}

#[derive(Clone, Copy, PartialEq, Eq)]
pub enum BinaryOperator {
	Add,
	Subtract,
	Multiply,
	Divide,
	Modulo,
	ShiftLeft,
	ShiftRight,
	BitAnd,
	BitOr,
	BitXor,
	And,
	Or,
	Equal,
	NotEqual,
	Less,
	Greater,
	LessEqual,
	GreaterEqual,
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
			TokenKind::VarKw => self.parse_local_declaration(),
			TokenKind::LBrace => self.parse_block(),

			_ => {
				if self.lookahead() == TokenKind::ColonEqual {
					return self.parse_local_definition();
				}

				let lhs = self.parse_expression();

				let operator_kind = self.current();
				let operator = match assignment_token_kind_to_operator(operator_kind) {
					Some(operator) => operator,
					None => return Statement::Expression(lhs),
				};
				self.bump(operator_kind);

				let rhs = self.parse_expression();

				match operator {
					Some(operator) => Statement::Assignment {
						lhs: lhs.clone(),
						rhs: Expression::Binary {
							lhs: Box::new(lhs),
							operator,
							rhs: Box::new(rhs),
						},
					},

					None => Statement::Assignment { lhs, rhs },
				}
			}
		}
	}

	fn parse_local_declaration(&mut self) -> Statement {
		self.bump(TokenKind::VarKw);

		let name = self.expect_text(TokenKind::Identifier);
		let ty = self.parse_ty();

		Statement::LocalDeclaration { name, ty }
	}

	fn parse_local_definition(&mut self) -> Statement {
		let name = self.expect_text(TokenKind::Identifier);
		self.bump(TokenKind::ColonEqual);

		let value = self.parse_expression();

		Statement::LocalDefinition { name, value }
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
		self.parse_expression_bp(0)
	}

	fn parse_expression_bp(&mut self, bp: u8) -> Expression {
		let mut lhs = self.parse_atom();

		loop {
			let operator_kind = self.current();
			let operator = match token_kind_to_operator(operator_kind) {
				Some(o) => o,
				None => break,
			};
			let right_bp = operator_to_bp(operator);

			if right_bp <= bp {
				break;
			}

			self.bump(operator_kind);

			lhs = Expression::Binary {
				lhs: Box::new(lhs),
				operator,
				rhs: Box::new(self.parse_expression_bp(right_bp)),
			};
		}

		lhs
	}

	fn parse_atom(&mut self) -> Expression {
		match self.current() {
			TokenKind::Integer => {
				let text = self.expect_text(TokenKind::Integer);
				Expression::Integer(text.parse().unwrap())
			}

			TokenKind::Identifier => {
				let text = self.expect_text(TokenKind::Identifier);
				Expression::Variable(text)
			}

			TokenKind::TrueKw => {
				self.bump(TokenKind::TrueKw);
				Expression::True
			}

			TokenKind::FalseKw => {
				self.bump(TokenKind::FalseKw);
				Expression::False
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

fn token_kind_to_operator(kind: TokenKind) -> Option<BinaryOperator> {
	Some(match kind {
		TokenKind::Plus => BinaryOperator::Add,
		TokenKind::Minus => BinaryOperator::Subtract,
		TokenKind::Star => BinaryOperator::Multiply,
		TokenKind::Slash => BinaryOperator::Divide,
		TokenKind::Percent => BinaryOperator::Modulo,
		TokenKind::LessLess => BinaryOperator::ShiftLeft,
		TokenKind::GreaterGreater => BinaryOperator::ShiftRight,
		TokenKind::And => BinaryOperator::BitAnd,
		TokenKind::Pipe => BinaryOperator::BitOr,
		TokenKind::Caret => BinaryOperator::BitXor,
		TokenKind::AndAnd => BinaryOperator::And,
		TokenKind::PipePipe => BinaryOperator::Or,
		TokenKind::EqualEqual => BinaryOperator::Equal,
		TokenKind::BangEqual => BinaryOperator::NotEqual,
		TokenKind::Less => BinaryOperator::Less,
		TokenKind::Greater => BinaryOperator::Greater,
		TokenKind::LessEqual => BinaryOperator::LessEqual,
		TokenKind::GreaterEqual => BinaryOperator::GreaterEqual,
		_ => return None,
	})
}

fn assignment_token_kind_to_operator(kind: TokenKind) -> Option<Option<BinaryOperator>> {
	if kind == TokenKind::Equal {
		return Some(None);
	}

	let operator = match kind {
		TokenKind::PlusEqual => BinaryOperator::Add,
		TokenKind::MinusEqual => BinaryOperator::Subtract,
		TokenKind::StarEqual => BinaryOperator::Multiply,
		TokenKind::SlashEqual => BinaryOperator::Divide,
		TokenKind::PercentEqual => BinaryOperator::Modulo,
		TokenKind::LessLessEqual => BinaryOperator::ShiftLeft,
		TokenKind::GreaterGreaterEqual => BinaryOperator::ShiftRight,
		TokenKind::AndEqual => BinaryOperator::BitAnd,
		TokenKind::PipeEqual => BinaryOperator::BitOr,
		TokenKind::CaretEqual => BinaryOperator::BitXor,
		TokenKind::AndAndEqual => BinaryOperator::And,
		TokenKind::PipePipeEqual => BinaryOperator::Or,
		_ => return None,
	};

	Some(Some(operator))
}

fn operator_to_bp(operator: BinaryOperator) -> u8 {
	match operator {
		BinaryOperator::Multiply
		| BinaryOperator::Divide
		| BinaryOperator::Modulo
		| BinaryOperator::ShiftLeft
		| BinaryOperator::ShiftRight
		| BinaryOperator::BitAnd => 5,

		BinaryOperator::Add
		| BinaryOperator::Subtract
		| BinaryOperator::BitOr
		| BinaryOperator::BitXor => 4,

		BinaryOperator::Equal
		| BinaryOperator::NotEqual
		| BinaryOperator::Less
		| BinaryOperator::Greater
		| BinaryOperator::LessEqual
		| BinaryOperator::GreaterEqual => 3,

		BinaryOperator::And => 2,

		BinaryOperator::Or => 1,
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

		self.s(")");

		if let Some(return_ty) = &proc.return_ty {
			self.s(" ");
			self.print_ty(return_ty);
			self.s(" ");
		}

		if proc.body == Statement::Block(Vec::new()) {
			if proc.return_ty.is_none() {
				self.s(" ");
			}
			self.s("{}");
		} else {
			self.newline();
			self.print_statement(&proc.body);
		}

		self.newline()
	}

	fn print_statement(&mut self, statement: &Statement) {
		match statement {
			Statement::LocalDeclaration { name, ty } => {
				self.s("var ");
				self.s(name);
				self.s(" ");
				self.print_ty(ty);
			}

			Statement::LocalDefinition { name, value } => {
				self.s(name);
				self.s(" := ");
				self.print_expression(value);
			}

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

			Statement::Expression(e) => self.print_expression(e),

			Statement::Assignment { lhs, rhs } => {
				self.print_expression(lhs);
				self.s(" = ");
				self.print_expression(rhs);
			}
		}
	}

	fn print_expression(&mut self, expression: &Expression) {
		match expression {
			Expression::Integer(i) => self.s(&format!("{i}")),

			Expression::Variable(name) => self.s(name),

			Expression::True => self.s("true"),
			Expression::False => self.s("false"),

			Expression::Binary { lhs, operator, rhs } => {
				self.s("(");
				self.print_expression(lhs);
				self.s(" ");

				let op = match operator {
					BinaryOperator::Add => "+",
					BinaryOperator::Subtract => "-",
					BinaryOperator::Multiply => "*",
					BinaryOperator::Divide => "/",
					BinaryOperator::Modulo => "%",
					BinaryOperator::ShiftLeft => "<<",
					BinaryOperator::ShiftRight => ">>",
					BinaryOperator::BitAnd => "&",
					BinaryOperator::BitOr => "|",
					BinaryOperator::BitXor => "^",
					BinaryOperator::And => "&&",
					BinaryOperator::Or => "||",
					BinaryOperator::Equal => "==",
					BinaryOperator::NotEqual => "!=",
					BinaryOperator::Less => "<",
					BinaryOperator::Greater => ">",
					BinaryOperator::LessEqual => "<=",
					BinaryOperator::GreaterEqual => ">=",
				};
				self.s(op);

				self.s(" ");
				self.print_expression(rhs);
				self.s(")");
			}
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
