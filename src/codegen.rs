use la_arena::Idx;

use crate::{hir, resolver};

pub fn codegen(index: &resolver::Index, hir: &hir::Hir) -> String {
	CodegenCtx { index, hir, buf: String::new(), indentation: 0 }.gen()
}

struct CodegenCtx<'a> {
	index: &'a resolver::Index,
	hir: &'a hir::Hir,
	buf: String,
	indentation: usize,
}

impl CodegenCtx<'_> {
	fn gen(mut self) -> String {
		for (name, procedure) in &self.index.procedures {
			self.gen_procedure_declaration(name, procedure);
			self.s(";");
			self.newline();
		}

		for (name, procedure) in &self.hir.procedures {
			self.newline();
			let index_procedure = &self.index.procedures[name];
			self.gen_procedure(name, procedure, index_procedure);
			self.newline();
		}

		self.buf
	}

	fn gen_procedure_declaration(&mut self, name: &str, procedure: &resolver::Procedure) {
		match &procedure.return_ty {
			Some(return_ty) => self.gen_declaration(name, return_ty),
			None => {
				self.s("void ");
				self.s(name);
			}
		}

		if procedure.parameters.is_empty() {
			self.s("(void)");
		} else {
			self.s("(");

			for (i, parameter) in procedure.parameters.iter().enumerate() {
				if i != 0 {
					self.s(", ");
				}

				self.gen_declaration(&parameter.name, &parameter.ty);
			}

			self.s(")");
		}
	}

	fn gen_procedure(
		&mut self,
		name: &str,
		procedure: &hir::Procedure,
		index_procedure: &resolver::Procedure,
	) {
		self.gen_procedure_declaration(name, index_procedure);

		self.newline();
		self.s("{");
		self.indentation += 1;

		let mut did_print_variables = false;

		for variable in procedure.storage.variables.values() {
			if variable.is_parameter {
				continue;
			}

			self.newline();
			self.gen_declaration(&variable.name, &variable.ty);
			self.s(";");
			did_print_variables = true;
		}

		if did_print_variables {
			self.newline();
		}

		match &procedure.storage.statements[procedure.body] {
			hir::Statement::Block(body) => {
				for s in body {
					self.newline();
					self.gen_statement(*s, &procedure.storage);
				}
			}

			_ => unreachable!(),
		}

		self.indentation -= 1;
		self.newline();
		self.s("}");
	}

	fn gen_statement(&mut self, statement: Idx<hir::Statement>, storage: &hir::BodyStorage) {
		match &storage.statements[statement] {
			hir::Statement::Assignment { lhs, rhs } => {
				self.gen_expression(*lhs, storage);
				self.s(" = ");
				self.gen_expression(*rhs, storage);
				self.s(";");
			}

			hir::Statement::If { condition, true_branch, false_branch } => {
				self.s("if (");
				self.gen_expression(*condition, storage);
				self.s(") ");
				self.gen_statement(*true_branch, storage);

				if let Some(false_branch) = false_branch {
					self.s(" else ");
					self.gen_statement(*false_branch, storage);
				}
			}

			hir::Statement::Loop { body } => {
				self.s("for (;;) ");
				self.gen_statement(*body, storage);
			}

			hir::Statement::Break => self.s("break;"),

			hir::Statement::Return { value } => {
				self.s("return");

				if let Some(value) = value {
					self.s(" ");
					self.gen_expression(*value, storage);
				}

				self.s(";");
			}

			hir::Statement::Block(body) => {
				self.s("{");
				self.indentation += 1;

				for s in body {
					self.newline();
					self.gen_statement(*s, storage);
				}

				self.indentation -= 1;
				self.newline();
				self.s("}");
			}

			hir::Statement::Expression(e) => {
				self.gen_expression(*e, storage);
				self.s(";");
			}

			hir::Statement::Call { name, arguments } => {
				self.s(name);
				self.s("(");

				for (i, argument) in arguments.iter().enumerate() {
					if i != 0 {
						self.s(", ");
					}

					self.gen_expression(*argument, storage);
				}

				self.s(")");
			}
		}
	}

	fn gen_expression(&mut self, expression: Idx<hir::Expression>, storage: &hir::BodyStorage) {
		match &storage.expressions[expression] {
			hir::Expression::Integer(i) => self.s(&i.to_string()),

			hir::Expression::Variable(variable) => self.s(&storage.variables[*variable].name),

			hir::Expression::Call { name, arguments } => {
				self.s(name);
				self.s("(");

				for (i, argument) in arguments.iter().enumerate() {
					if i != 0 {
						self.s(", ");
					}

					self.gen_expression(*argument, storage);
				}

				self.s(")");
			}

			hir::Expression::True => self.s("true"),

			hir::Expression::False => self.s("false"),

			hir::Expression::Binary { lhs, rhs, op } => {
				self.s("(");
				self.gen_expression(*lhs, storage);
				self.s(" ");
				self.s(&op.to_string());
				self.s(" ");
				self.gen_expression(*rhs, storage);
				self.s(")");
			}
		}
	}

	fn gen_declaration(&mut self, name: &str, ty: &resolver::Ty) {
		match ty {
			resolver::Ty::Int => {
				self.s("int ");
				self.s(name);
			}
			resolver::Ty::Bool => {
				self.s("bool ");
				self.s(name);
			}
			resolver::Ty::Named(n) => {
				self.s(n);
				self.s(" ");
				self.s(name);
			}
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
	use std::{fs, path::PathBuf, process::Command};

	crate::testing::run_tests("test_data_codegen", |input| {
		let ast = crate::parser::parse(input, PathBuf::from("test"));
		let index = crate::indexer::index(&ast);
		let index = crate::resolver::resolve(&index);
		let hir = crate::sema::analyze(&ast, &index);
		let c_code = codegen(&index, &hir);

		fs::write("test_tmp_code.c", c_code).unwrap();
		Command::new("clang")
			.arg("-ftrivial-auto-var-init=zero")
			.arg("-fwrapv")
			.arg("-o")
			.arg("test_tmp_out")
			.arg("test_tmp_code.c")
			.status()
			.unwrap();

		let output = Command::new("./test_tmp_out").output().unwrap();
		let stdout = String::from_utf8_lossy(&output.stdout);

		fs::remove_file("test_tmp_code.c").unwrap();
		fs::remove_file("test_tmp_out").unwrap();

		format!("{stdout}{}\n", output.status)
	});
}
