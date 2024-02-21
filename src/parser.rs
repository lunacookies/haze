use std::{cell::Cell, path::PathBuf};

use crate::{
	ast::{
		Ast, BinaryOperator, Definition, DefinitionKind, Expression, ExpressionKind, Field,
		Function, Parameter, Statement, StatementKind, Struct, Ty, TyKind, UnaryOperator,
	},
	lexer::{lex, Loc, Token, TokenKind},
};

pub fn parse(input: &str, file: PathBuf) -> Ast {
	let tokens = lex(input, file);
	Parser::new(tokens).parse()
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
			self.expect(TokenKind::SemiOrNewline);
		}

		Ast { definitions }
	}

	fn parse_definition(&mut self) -> Definition {
		match self.current() {
			TokenKind::FuncKw => self.parse_function(),
			TokenKind::StructKw => self.parse_struct(),
			_ => self.error("expected definition".to_string()),
		}
	}

	fn parse_function(&mut self) -> Definition {
		let loc = self.current_loc();
		self.bump(TokenKind::FuncKw);

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

		let return_ty = if self.current().can_start_ty() { Some(self.parse_ty()) } else { None };

		let mut is_extern = false;
		if self.eat(TokenKind::Hash) {
			let loc = self.current_loc();
			let directive = self.expect_text(TokenKind::Identifier);
			match directive.as_str() {
				"extern" => is_extern = true,
				_ => crate::error(loc, format!("invalid function directive “#{directive}”")),
			}
		}

		let body = if self.at(TokenKind::LBrace) { Some(self.parse_block()) } else { None };

		Definition {
			kind: DefinitionKind::Function(Function {
				name,
				parameters,
				return_ty,
				body,
				is_extern,
			}),
			loc,
		}
	}

	fn parse_struct(&mut self) -> Definition {
		let loc = self.current_loc();
		self.bump(TokenKind::StructKw);

		let name = self.expect_text(TokenKind::Identifier);

		self.expect(TokenKind::LBrace);
		let mut fields = Vec::new();

		while !self.at_eof() && !self.at(TokenKind::RBrace) {
			let field_name = self.expect_text(TokenKind::Identifier);
			let field_ty = self.parse_ty();
			self.expect(TokenKind::SemiOrNewline);
			fields.push(Field { name: field_name, ty: field_ty });
		}

		self.expect(TokenKind::RBrace);

		Definition { kind: DefinitionKind::Struct(Struct { name, fields }), loc }
	}

	fn parse_statement(&mut self) -> Statement {
		let s = match self.current() {
			TokenKind::VarKw => self.parse_local_declaration(),
			TokenKind::IfKw => self.parse_if(),
			TokenKind::ForKw => self.parse_loop(),
			TokenKind::BreakKw => self.parse_break(),
			TokenKind::ReturnKw => self.parse_return(),
			TokenKind::LBrace => self.parse_block(),
			_ => self.parse_complex_statement(),
		};

		assert!(
			self.tokens[self.cursor - 1].kind.can_end_statement(),
			"{:?} cannot end a statement",
			self.tokens[self.cursor - 1].kind
		);

		s
	}

	fn parse_complex_statement(&mut self) -> Statement {
		if self.lookahead() == TokenKind::ColonEqual {
			return self.parse_local_definition();
		}

		let loc = self.current_loc();
		let lhs = self.parse_expression();

		let operator_loc = self.current_loc();
		let operator_kind = self.current();
		let assignment_operator = match assignment_token_kind_to_operator(operator_kind) {
			Some(o) => o,
			None => return Statement { kind: StatementKind::Expression(lhs), loc },
		};
		self.bump(operator_kind);

		match assignment_operator {
			AssignmentOperator::Assignment => {
				let rhs = self.parse_expression();
				Statement { kind: StatementKind::Assignment { lhs, rhs }, loc }
			}

			AssignmentOperator::CompoundAssignment(operator) => {
				let rhs = self.parse_expression();

				Statement {
					kind: StatementKind::Assignment {
						lhs: lhs.clone(),
						rhs: Expression {
							kind: ExpressionKind::Binary {
								lhs: Box::new(lhs),
								operator,
								rhs: Box::new(rhs),
							},
							loc: loc.clone(),
						},
					},
					loc,
				}
			}

			AssignmentOperator::IncDecrement(operator) => {
				let rhs = Expression { kind: ExpressionKind::Integer(1), loc: operator_loc };

				Statement {
					kind: StatementKind::Assignment {
						lhs: lhs.clone(),
						rhs: Expression {
							kind: ExpressionKind::Binary {
								lhs: Box::new(lhs),
								operator,
								rhs: Box::new(rhs),
							},
							loc: loc.clone(),
						},
					},
					loc,
				}
			}
		}
	}

	fn parse_local_declaration(&mut self) -> Statement {
		let loc = self.current_loc();
		self.bump(TokenKind::VarKw);

		let name = self.expect_text(TokenKind::Identifier);
		let ty = self.parse_ty();

		Statement { kind: StatementKind::LocalDeclaration { name, ty }, loc }
	}

	fn parse_local_definition(&mut self) -> Statement {
		let loc = self.current_loc();
		let name = self.expect_text(TokenKind::Identifier);
		self.bump(TokenKind::ColonEqual);

		let value = self.parse_expression();

		Statement { kind: StatementKind::LocalDefinition { name, value }, loc }
	}

	fn parse_if(&mut self) -> Statement {
		let loc = self.current_loc();
		self.bump(TokenKind::IfKw);

		let condition = self.parse_expression();
		let true_branch = self.parse_block();
		let false_branch =
			if self.eat(TokenKind::ElseKw) { Some(self.parse_statement()) } else { None };

		Statement {
			kind: StatementKind::If {
				condition,
				true_branch: Box::new(true_branch),
				false_branch: false_branch.map(Box::new),
			},
			loc,
		}
	}

	fn parse_loop(&mut self) -> Statement {
		let loc = self.current_loc();
		self.bump(TokenKind::ForKw);

		let mut initializer = None;
		let mut condition = None;
		let mut post = None;

		if self.lookahead() == TokenKind::ColonEqual {
			initializer = Some(self.parse_local_definition());
			self.expect(TokenKind::SemiOrNewline);

			condition = Some(self.parse_expression());
			self.expect(TokenKind::SemiOrNewline);

			post = Some(self.parse_statement())
		} else if !self.at(TokenKind::LBrace) {
			condition = Some(self.parse_expression());
		}

		let body = self.parse_block();

		Statement {
			kind: StatementKind::Loop {
				initializer: initializer.map(Box::new),
				condition,
				post: post.map(Box::new),
				body: Box::new(body),
			},
			loc,
		}
	}

	fn parse_break(&mut self) -> Statement {
		let loc = self.current_loc();
		self.bump(TokenKind::BreakKw);

		Statement { kind: StatementKind::Break, loc }
	}

	fn parse_return(&mut self) -> Statement {
		let loc = self.current_loc();
		self.bump(TokenKind::ReturnKw);

		let value = if self.current().can_start_expression() {
			Some(self.parse_expression())
		} else {
			None
		};

		Statement { kind: StatementKind::Return { value }, loc }
	}

	fn parse_block(&mut self) -> Statement {
		let loc = self.current_loc();

		self.expect(TokenKind::LBrace);

		let mut statements = Vec::new();
		while !self.at_eof() && !self.at(TokenKind::RBrace) {
			statements.push(self.parse_statement());
			self.expect(TokenKind::SemiOrNewline);
		}

		self.expect(TokenKind::RBrace);

		Statement { kind: StatementKind::Block(statements), loc }
	}

	fn parse_expression(&mut self) -> Expression {
		self.parse_expression_bp(0)
	}

	fn parse_expression_bp(&mut self, bp: u8) -> Expression {
		let mut lhs = self.parse_lhs();

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

			let loc = lhs.loc.clone();
			lhs = Expression {
				kind: ExpressionKind::Binary {
					lhs: Box::new(lhs),
					operator,
					rhs: Box::new(self.parse_expression_bp(right_bp)),
				},
				loc,
			};
		}

		lhs
	}

	fn parse_lhs(&mut self) -> Expression {
		let mut lhs = self.parse_atom();

		loop {
			match self.current() {
				TokenKind::Dot => {
					self.bump(TokenKind::Dot);

					let field = self.expect_text(TokenKind::Identifier);
					let loc = lhs.loc.clone();
					lhs = Expression {
						kind: ExpressionKind::FieldAccess { lhs: Box::new(lhs), field },
						loc,
					};
				}

				TokenKind::LBracket => {
					self.bump(TokenKind::LBracket);

					let index = self.parse_expression();
					self.expect(TokenKind::RBracket);

					let loc = lhs.loc.clone();
					lhs = Expression {
						kind: ExpressionKind::Indexing {
							lhs: Box::new(lhs),
							index: Box::new(index),
						},
						loc,
					};
				}

				_ => break,
			}
		}

		lhs
	}

	fn parse_atom(&mut self) -> Expression {
		if !self.current().can_start_expression() {
			self.error("expected expression".to_string());
		}

		match self.current() {
			TokenKind::Integer => {
				let loc = self.current_loc();
				let text = self.expect_text(TokenKind::Integer);

				Expression { kind: ExpressionKind::Integer(text.parse().unwrap()), loc }
			}

			TokenKind::QuotedString => {
				let loc = self.current_loc();
				let text = self.expect_text(TokenKind::QuotedString);

				let mut out = Vec::new();
				let mut in_escape = false;

				for &b in text[1..text.len() - 1].as_bytes() {
					if in_escape {
						out.push(match b {
							b'\\' => b'\\',
							b'n' => b'\n',
							b't' => b'\t',
							b'"' => b'"',
							_ => unreachable!(),
						});
						in_escape = false;
						continue;
					}

					if b == b'\\' {
						in_escape = true;
						continue;
					}

					out.push(b);
				}

				let string = String::from_utf8(out).unwrap();
				Expression { kind: ExpressionKind::String(string), loc }
			}

			TokenKind::Identifier if self.lookahead() == TokenKind::LParen => {
				let loc = self.current_loc();
				let name = self.expect_text(TokenKind::Identifier);
				self.bump(TokenKind::LParen);

				let mut arguments = Vec::new();

				while !self.at_eof() && !self.at(TokenKind::RParen) {
					arguments.push(self.parse_expression());

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

				Expression { kind: ExpressionKind::Call { name, arguments }, loc }
			}

			TokenKind::Identifier => {
				let loc = self.current_loc();
				let text = self.expect_text(TokenKind::Identifier);

				Expression { kind: ExpressionKind::Variable(text), loc }
			}

			TokenKind::TrueKw => {
				let loc = self.current_loc();
				self.bump(TokenKind::TrueKw);

				Expression { kind: ExpressionKind::True, loc }
			}

			TokenKind::FalseKw => {
				let loc = self.current_loc();
				self.bump(TokenKind::FalseKw);

				Expression { kind: ExpressionKind::False, loc }
			}

			TokenKind::And => {
				let loc = self.current_loc();
				self.bump(TokenKind::And);
				let operand = self.parse_lhs();

				Expression { kind: ExpressionKind::AddressOf(Box::new(operand)), loc }
			}

			TokenKind::Star => {
				let loc = self.current_loc();
				self.bump(TokenKind::Star);
				let operand = self.parse_lhs();

				Expression { kind: ExpressionKind::Dereference(Box::new(operand)), loc }
			}

			TokenKind::Bang => {
				let loc = self.current_loc();
				self.bump(TokenKind::Bang);
				let operand = self.parse_lhs();

				Expression {
					kind: ExpressionKind::Unary {
						operand: Box::new(operand),
						operator: UnaryOperator::Not,
					},
					loc,
				}
			}

			TokenKind::Tilde => {
				let loc = self.current_loc();
				self.bump(TokenKind::Tilde);
				let operand = self.parse_lhs();

				Expression {
					kind: ExpressionKind::Unary {
						operand: Box::new(operand),
						operator: UnaryOperator::BitNot,
					},
					loc,
				}
			}

			TokenKind::CastKw => {
				let loc = self.current_loc();
				self.bump(TokenKind::CastKw);

				self.expect(TokenKind::LParen);
				let ty = self.parse_ty();
				self.expect(TokenKind::RParen);
				let operand = self.parse_lhs();

				Expression { kind: ExpressionKind::Cast { ty, operand: Box::new(operand) }, loc }
			}

			_ => unreachable!(),
		}
	}

	fn parse_ty(&mut self) -> Ty {
		if !self.current().can_start_ty() {
			self.error("expected type".to_string())
		}

		let loc = self.current_loc();

		match self.current() {
			TokenKind::Star => {
				self.bump(TokenKind::Star);
				let pointee = self.parse_ty();
				Ty { kind: TyKind::Pointer { pointee: Box::new(pointee) }, loc }
			}

			TokenKind::Identifier => {
				let text = self.expect_text(TokenKind::Identifier);

				match text.as_str() {
					"int" => Ty { kind: TyKind::Int, loc },
					"byte" => Ty { kind: TyKind::Byte, loc },
					"bool" => Ty { kind: TyKind::Bool, loc },
					_ => Ty { kind: TyKind::Named(text), loc },
				}
			}

			_ => unreachable!(),
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

	fn current_loc(&self) -> Loc {
		self.tokens[self.cursor].loc.clone()
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

enum AssignmentOperator {
	Assignment,
	CompoundAssignment(BinaryOperator),
	IncDecrement(BinaryOperator),
}

fn assignment_token_kind_to_operator(kind: TokenKind) -> Option<AssignmentOperator> {
	if kind == TokenKind::Equal {
		return Some(AssignmentOperator::Assignment);
	}

	let operator = match kind {
		TokenKind::PlusPlus => return Some(AssignmentOperator::IncDecrement(BinaryOperator::Add)),
		TokenKind::MinusMinus => {
			return Some(AssignmentOperator::IncDecrement(BinaryOperator::Subtract))
		}

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

	Some(AssignmentOperator::CompoundAssignment(operator))
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

#[cfg(test)]
#[test]
fn tests() {
	crate::testing::run_tests("test_data_parser", |input| {
		let ast = parse(input, PathBuf::from("test"));
		ast.pretty_print()
	});
}
