use indexmap::IndexMap;
use la_arena::{Arena, Idx};

use crate::{ast, resolver::Ty};

#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub struct Hir {
	pub procedures: IndexMap<String, Procedure>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Procedure {
	pub storage: BodyStorage,
	pub body: Idx<Statement>,
}

#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub struct BodyStorage {
	pub variables: Arena<Variable>,
	pub statements: Arena<Statement>,
	pub expressions: Arena<Expression>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Variable {
	pub name: String,
	pub ty: Ty,
	pub is_parameter: bool,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Statement {
	Assignment {
		lhs: Idx<Expression>,
		rhs: Idx<Expression>,
	},
	If {
		condition: Idx<Expression>,
		true_branch: Idx<Statement>,
		false_branch: Option<Idx<Statement>>,
	},
	Loop {
		condition: Option<Idx<Expression>>,
		body: Idx<Statement>,
	},
	Break,
	Return {
		value: Option<Idx<Expression>>,
	},
	Block(Vec<Idx<Statement>>),
	Expression(Idx<Expression>),
	Call {
		name: String,
		arguments: Vec<Idx<Expression>>,
	},
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Expression {
	Integer(u64),
	Byte(u8),
	String(String),
	Variable(Idx<Variable>),
	Call { name: String, arguments: Vec<Idx<Expression>> },
	True,
	False,
	Binary { lhs: Idx<Expression>, rhs: Idx<Expression>, op: ast::BinaryOperator },
	FieldAccess { lhs: Idx<Expression>, field: String },
	Indexing { lhs: Idx<Expression>, index: Idx<Expression> },
	AddressOf(Idx<Expression>),
	Dereference(Idx<Expression>),
	Cast { ty: Ty, operand: Idx<Expression> },
}

impl Hir {
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
	fn print_ast(&mut self, hir: &Hir) {
		for (i, (name, proc)) in hir.procedures.iter().enumerate() {
			if i != 0 {
				self.newline();
			}

			self.print_procedure(name, proc);
		}
	}

	fn print_procedure(&mut self, name: &str, proc: &Procedure) {
		self.s("proc ");
		self.s(name);

		self.indentation += 1;

		for variable in proc.storage.variables.values() {
			self.newline();

			if variable.is_parameter {
				self.s("param ");
			} else {
				self.s("var ");
			}

			self.s(&variable.name);
			self.s(" ");
			self.s(&variable.ty.to_string());
		}

		self.indentation -= 1;
		self.newline();

		self.print_statement(proc.body, &proc.storage);
		self.newline();
	}

	fn print_statement(&mut self, statement: Idx<Statement>, storage: &BodyStorage) {
		match &storage.statements[statement] {
			Statement::Assignment { lhs, rhs } => {
				self.print_expression(*lhs, storage);
				self.s(" = ");
				self.print_expression(*rhs, storage);
			}

			Statement::If { condition, true_branch, false_branch } => {
				self.s("if ");
				self.print_expression(*condition, storage);
				self.s(" ");
				self.print_statement(*true_branch, storage);
				if let Some(false_branch) = false_branch {
					self.s(" else ");
					self.print_statement(*false_branch, storage);
				}
			}

			Statement::Loop { condition, body } => {
				self.s("for ");

				if let Some(c) = condition {
					self.print_expression(*c, storage);
					self.s(" ");
				}

				self.print_statement(*body, storage);
			}

			Statement::Break => self.s("break"),

			Statement::Return { value } => {
				self.s("return");
				if let Some(v) = value {
					self.s(" ");
					self.print_expression(*v, storage);
				}
			}

			Statement::Block(statements) => {
				self.s("{");
				self.indentation += 1;

				for s in statements {
					self.newline();
					self.print_statement(*s, storage);
				}

				self.indentation -= 1;
				self.newline();
				self.s("}");
			}

			Statement::Expression(e) => self.print_expression(*e, storage),

			Statement::Call { name, arguments } => {
				self.s("call ");
				self.print_call(name, arguments, storage)
			}
		}
	}

	fn print_expression(&mut self, expression: Idx<Expression>, storage: &BodyStorage) {
		match &storage.expressions[expression] {
			Expression::Integer(i) => self.s(&i.to_string()),
			Expression::Byte(i) => {
				self.s(&i.to_string());
				self.s("_byte");
			}

			Expression::String(s) => self.s(&format!("{s:?}")),

			Expression::Variable(v) => self.s(&storage.variables[*v].name),

			Expression::Call { name, arguments } => self.print_call(name, arguments, storage),

			Expression::True => self.s("true"),
			Expression::False => self.s("false"),

			Expression::Binary { lhs, rhs, op } => {
				self.s("(");
				self.print_expression(*lhs, storage);
				self.s(" ");
				self.s(&op.to_string());
				self.s(" ");
				self.print_expression(*rhs, storage);
				self.s(")");
			}

			Expression::FieldAccess { lhs, field } => {
				self.s("(");
				self.print_expression(*lhs, storage);
				self.s(".");
				self.s(field);
				self.s(")");
			}

			Expression::Indexing { lhs, index } => {
				self.s("(");
				self.print_expression(*lhs, storage);
				self.s("[");
				self.print_expression(*index, storage);
				self.s("])");
			}

			Expression::AddressOf(e) => {
				self.s("(&");
				self.print_expression(*e, storage);
				self.s(")");
			}

			Expression::Dereference(e) => {
				self.s("(*");
				self.print_expression(*e, storage);
				self.s(")");
			}

			Expression::Cast { ty, operand } => {
				self.s("cast(");
				self.s(&ty.to_string());
				self.s(", ");
				self.print_expression(*operand, storage);
				self.s(")");
			}
		}
	}

	fn print_call(&mut self, name: &str, arguments: &[Idx<Expression>], storage: &BodyStorage) {
		self.s(name);
		self.s("(");

		for (i, argument) in arguments.iter().enumerate() {
			if i != 0 {
				self.s(", ");
			}

			self.print_expression(*argument, storage);
		}

		self.s(")");
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
