use std::fmt;

use crate::lexer::Loc;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Ast {
	pub definitions: Vec<Definition>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Definition {
	pub kind: DefinitionKind,
	pub loc: Loc,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum DefinitionKind {
	Procedure(Procedure),
	Struct(Struct),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Procedure {
	pub name: String,
	pub parameters: Vec<Parameter>,
	pub return_ty: Option<Ty>,
	pub body: Option<Statement>,
	pub is_extern: bool,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Parameter {
	pub name: String,
	pub ty: Ty,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Struct {
	pub name: String,
	pub fields: Vec<Field>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Field {
	pub name: String,
	pub ty: Ty,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Statement {
	pub kind: StatementKind,
	pub loc: Loc,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum StatementKind {
	LocalDeclaration { name: String, ty: Ty },
	LocalDefinition { name: String, value: Expression },
	If { condition: Expression, true_branch: Box<Statement>, false_branch: Option<Box<Statement>> },
	Loop { body: Box<Statement> },
	Break,
	Return { value: Option<Expression> },
	Block(Vec<Statement>),
	Expression(Expression),
	Assignment { lhs: Expression, rhs: Expression },
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Expression {
	pub kind: ExpressionKind,
	pub loc: Loc,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ExpressionKind {
	Integer(u64),
	Variable(String),
	Call { name: String, arguments: Vec<Expression> },
	True,
	False,
	Binary { lhs: Box<Expression>, operator: BinaryOperator, rhs: Box<Expression> },
	FieldAccess { lhs: Box<Expression>, field: String },
	AddressOf(Box<Expression>),
	Dereference(Box<Expression>),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
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

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Ty {
	pub kind: TyKind,
	pub loc: Loc,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TyKind {
	Int,
	Bool,
	Named(String),
	Pointer { pointee: Box<Ty> },
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
		for (i, definition) in ast.definitions.iter().enumerate() {
			if i != 0 {
				self.newline();
			}

			match &definition.kind {
				DefinitionKind::Procedure(proc) => self.print_procedure(proc),
				DefinitionKind::Struct(strukt) => self.print_struct(strukt),
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
		}

		if proc.is_extern {
			self.s(" #extern");
		}

		if let Some(body) = &proc.body {
			self.newline();
			self.print_statement(body);
		}

		self.newline()
	}

	fn print_struct(&mut self, strukt: &Struct) {
		self.s("struct ");
		self.s(&strukt.name);
		self.newline();
		self.s("{");
		self.indentation += 1;

		for field in &strukt.fields {
			self.newline();
			self.s(&field.name);
			self.s(" ");
			self.print_ty(&field.ty);
		}

		self.indentation -= 1;
		self.newline();
		self.s("}");
		self.newline();
	}

	fn print_statement(&mut self, statement: &Statement) {
		match &statement.kind {
			StatementKind::LocalDeclaration { name, ty } => {
				self.s("var ");
				self.s(name);
				self.s(" ");
				self.print_ty(ty);
			}

			StatementKind::LocalDefinition { name, value } => {
				self.s(name);
				self.s(" := ");
				self.print_expression(value);
			}

			StatementKind::If { condition, true_branch, false_branch } => {
				self.s("if ");
				self.print_expression(condition);
				self.s(" ");
				self.print_statement(true_branch);
				if let Some(false_branch) = false_branch {
					self.s(" else ");
					self.print_statement(false_branch);
				}
			}

			StatementKind::Loop { body } => {
				self.s("for ");
				self.print_statement(body);
			}

			StatementKind::Break => self.s("break"),

			StatementKind::Return { value } => {
				self.s("return");

				if let Some(value) = value {
					self.s(" ");
					self.print_expression(value);
				}
			}

			StatementKind::Block(statements) => {
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

			StatementKind::Expression(e) => self.print_expression(e),

			StatementKind::Assignment { lhs, rhs } => {
				self.print_expression(lhs);
				self.s(" = ");
				self.print_expression(rhs);
			}
		}
	}

	fn print_expression(&mut self, expression: &Expression) {
		match &expression.kind {
			ExpressionKind::Integer(i) => self.s(&format!("{i}")),

			ExpressionKind::Variable(name) => self.s(name),

			ExpressionKind::Call { name, arguments } => {
				self.s(name);
				self.s("(");

				for (i, argument) in arguments.iter().enumerate() {
					if i != 0 {
						self.s(", ");
					}

					self.print_expression(argument);
				}

				self.s(")")
			}

			ExpressionKind::True => self.s("true"),
			ExpressionKind::False => self.s("false"),

			ExpressionKind::Binary { lhs, operator, rhs } => {
				self.s("(");
				self.print_expression(lhs);
				self.s(" ");
				self.s(&operator.to_string());
				self.s(" ");
				self.print_expression(rhs);
				self.s(")");
			}

			ExpressionKind::FieldAccess { lhs, field } => {
				self.s("(");
				self.print_expression(lhs);
				self.s(".");
				self.s(field);
				self.s(")");
			}

			ExpressionKind::AddressOf(e) => {
				self.s("(&");
				self.print_expression(e);
				self.s(")");
			}

			ExpressionKind::Dereference(e) => {
				self.s("(*");
				self.print_expression(e);
				self.s(")");
			}
		}
	}

	fn print_ty(&mut self, ty: &Ty) {
		match &ty.kind {
			TyKind::Int => self.s("int"),
			TyKind::Bool => self.s("bool"),
			TyKind::Named(n) => self.s(n),
			TyKind::Pointer { pointee } => {
				self.s("*");
				self.print_ty(pointee);
			}
		}
	}

	fn s(&mut self, s: &str) {
		assert!(!s.contains('\n'));
		self.buf.push_str(s);
	}

	fn newline(&mut self) {
		self.buf.push('\n');
		for _ in 0..self.indentation {
			self.buf.push('\t');
		}
	}
}

impl fmt::Display for BinaryOperator {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		let s = match self {
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
		write!(f, "{s}")
	}
}
