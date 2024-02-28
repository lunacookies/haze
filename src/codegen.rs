use la_arena::{ArenaMap, Idx};

use crate::{hir, resolver};

pub fn codegen(index: &resolver::Index, hir: &hir::Hir) -> String {
	CodegenCtx {
		index,
		hir,
		buf: String::new(),
		indentation: 0,
		disambiguated_variable_names: ArenaMap::new(),
	}
	.gen()
}

struct CodegenCtx<'a> {
	index: &'a resolver::Index,
	hir: &'a hir::Hir,
	buf: String,
	indentation: usize,
	disambiguated_variable_names: ArenaMap<Idx<hir::Variable>, String>,
}

impl CodegenCtx<'_> {
	fn gen(mut self) -> String {
		self.s("#include <stdbool.h>");
		self.newline();
		self.s("#include <stdint.h>");
		self.newline();

		self.s("struct __slice { uint8_t *data; int count; };");
		self.newline();

		self.s("void *__slice_at(struct __slice s, int i)");
		self.newline();
		self.s("{");
		self.indentation += 1;
		self.newline();
		self.s("if (i < 0 || i >= s.count) __builtin_debugtrap();");
		self.newline();
		self.s("return s.data + i;");
		self.indentation -= 1;
		self.newline();
		self.s("}");
		self.newline();

		self.s("struct __slice __slice_slice(struct __slice s, int start, int end)");
		self.newline();
		self.s("{");
		self.indentation += 1;
		self.newline();
		self.s("if (end < start || start < 0 || end < 0 || start > s.count || end > s.count) {");
		self.indentation += 1;
		self.newline();
		self.s("__builtin_debugtrap();");
		self.indentation -= 1;
		self.newline();
		self.s("}");
		self.newline();
		self.s("return (struct __slice){ .data = s.data + start, .count = end - start };");
		self.indentation -= 1;
		self.newline();
		self.s("}");
		self.newline();

		self.s("struct __slice __slice_many_pointer(uint8_t *p, int start, int end)");
		self.newline();
		self.s("{");
		self.indentation += 1;
		self.newline();
		self.s("if (end < start) {");
		self.indentation += 1;
		self.newline();
		self.s("__builtin_debugtrap();");
		self.indentation -= 1;
		self.newline();
		self.s("}");
		self.newline();
		self.s("return (struct __slice){ .data = p + start, .count = end - start };");
		self.indentation -= 1;
		self.newline();
		self.s("}");
		self.newline();

		for (name, ty) in &self.index.named_tys {
			if name == "__slice" {
				continue; // hack for test_data_codegen/out_of_bounds_slicing
			}

			self.gen_ty(name, ty);
			self.newline();
		}

		for (name, function) in &self.index.functions {
			self.gen_function_declaration(name, function);
			self.s(";");
			self.newline();
		}

		for (name, function) in &self.hir.functions {
			self.newline();
			let index_function = &self.index.functions[name];
			self.gen_function(name, function, index_function);
			self.newline();
		}

		self.buf
	}

	fn gen_ty(&mut self, name: &str, ty: &resolver::NamedTy) {
		match ty {
			resolver::NamedTy::Struct(strukt) => {
				self.s("struct ");
				self.s(name);
				self.s(" {");
				self.indentation += 1;

				for field in &strukt.fields {
					self.newline();
					self.gen_declaration(&field.name, &field.ty);
					self.s(";");
				}

				self.indentation -= 1;
				self.newline();
				self.s("};");
				self.newline();
			}
		}
	}

	fn gen_function_declaration(&mut self, name: &str, function: &resolver::Function) {
		match &function.return_ty {
			Some(return_ty) => self.gen_declaration(name, return_ty),
			None => {
				self.s("void ");
				self.s(name);
			}
		}

		if function.parameters.is_empty() {
			self.s("(void)");
		} else {
			self.s("(");

			for (i, parameter) in function.parameters.iter().enumerate() {
				if i != 0 {
					self.s(", ");
				}

				self.gen_declaration(&parameter.name, &parameter.ty);
			}

			self.s(")");
		}
	}

	fn gen_function(
		&mut self,
		name: &str,
		function: &hir::Function,
		index_function: &resolver::Function,
	) {
		self.gen_function_declaration(name, index_function);

		self.newline();
		self.s("{");
		self.indentation += 1;

		self.disambiguated_variable_names.clear();
		hir::disambiguated_variable_names(function, &mut self.disambiguated_variable_names);
		let mut did_print_variables = false;

		for (variable_idx, variable) in function.storage.variables.iter() {
			if variable.is_parameter {
				continue;
			}

			let name = self.disambiguated_variable_names[variable_idx].clone();
			self.newline();
			self.gen_declaration(&name, &variable.ty);
			self.s(";");
			did_print_variables = true;
		}

		if did_print_variables {
			self.newline();
		}

		match &function.storage.statements[function.body] {
			hir::Statement::Block(body) => {
				for s in body {
					self.newline();
					self.gen_statement(*s, &function.storage);
				}
			}

			_ => unreachable!(),
		}

		self.indentation -= 1;
		self.newline();
		self.s("}");
	}

	fn gen_statement(&mut self, statement: Idx<hir::Statement>, storage: &hir::BodyStorage) {
		self.gen_statement_semi(statement, storage, true);
	}

	fn gen_statement_semi(
		&mut self,
		statement: Idx<hir::Statement>,
		storage: &hir::BodyStorage,
		semi: bool,
	) {
		match &storage.statements[statement] {
			hir::Statement::Assignment { lhs, rhs } => {
				self.gen_expression(*lhs, storage);
				self.s(" = ");
				self.gen_expression(*rhs, storage);

				if semi {
					self.s(";");
				}
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

			hir::Statement::Loop { initializer, condition, post, body } => {
				self.s("for (");

				if let Some(i) = initializer {
					self.gen_statement_semi(*i, storage, false);
				}

				self.s(";");

				if let Some(c) = condition {
					self.gen_expression(*c, storage);
				}

				self.s(";");

				if let Some(p) = post {
					self.gen_statement_semi(*p, storage, false);
				}

				self.s(") ");
				self.gen_statement(*body, storage);
			}

			hir::Statement::Break => {
				self.s("break");
				if semi {
					self.s(";")
				}
			}

			hir::Statement::Return { value } => {
				self.s("return");

				if let Some(value) = value {
					self.s(" ");
					self.gen_expression(*value, storage);
				}

				if semi {
					self.s(";");
				}
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

				if semi {
					self.s(";");
				}
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

				if semi {
					self.s(";");
				}
			}
		}
	}

	fn gen_expression(&mut self, expression: Idx<hir::Expression>, storage: &hir::BodyStorage) {
		match &storage.expressions[expression] {
			hir::Expression::Integer(i) => self.s(&i.to_string()),

			hir::Expression::Byte(i) => self.s(&i.to_string()),

			hir::Expression::String(s) => {
				self.s("(struct __slice){");
				self.indentation += 1;
				self.newline();

				self.s(".data = (uint8_t[]){ ");

				for (i, b) in s.as_bytes().iter().enumerate() {
					if i != 0 {
						self.s(", ");
					}

					self.s(&b.to_string());
				}

				self.s(" },");
				self.newline();

				self.s(".count = ");
				self.s(&s.len().to_string());
				self.s(",");

				self.indentation -= 1;
				self.newline();
				self.s("}");
			}

			hir::Expression::Variable(variable) => {
				let name = self.disambiguated_variable_names[*variable].clone();
				self.s(&name);
			}

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

			hir::Expression::Binary { lhs, rhs, operator } => {
				self.s("(");
				self.gen_expression(*lhs, storage);
				self.s(" ");
				self.s(&operator.to_string());
				self.s(" ");
				self.gen_expression(*rhs, storage);
				self.s(")");
			}

			hir::Expression::Unary { operand, operator } => {
				self.s("(");
				self.s(&operator.to_string());
				self.gen_expression(*operand, storage);
				self.s(")");
			}

			hir::Expression::FieldAccess { lhs, field } => {
				self.s("(");
				self.gen_expression(*lhs, storage);
				self.s(".");
				self.s(field);
				self.s(")");
			}

			hir::Expression::SliceData { slice, element_ty } => {
				let pointer_ty =
					resolver::Ty::ManyPointer { pointee: Box::new(element_ty.clone()) };
				self.s("((");
				self.gen_declaration("", &pointer_ty);
				self.s(")(");
				self.gen_expression(*slice, storage);
				self.s(".data))");
			}

			hir::Expression::SliceCount { slice, element_ty } => {
				self.s("(");
				self.gen_expression(*slice, storage);
				self.s(".count / sizeof(");
				self.gen_declaration("", element_ty);
				self.s("))");
			}

			hir::Expression::ManyPointerIndexing { lhs, index } => {
				self.s("(");
				self.gen_expression(*lhs, storage);
				self.s("[");
				self.gen_expression(*index, storage);
				self.s("])");
			}

			hir::Expression::SliceIndexing { slice, index, element_ty } => {
				let pointer_ty =
					resolver::Ty::SinglePointer { pointee: Box::new(element_ty.clone()) };
				self.s("(*(");
				self.gen_declaration("", &pointer_ty);
				self.s(")__slice_at(");
				self.gen_expression(*slice, storage);
				self.s(", (int)sizeof(");
				self.gen_declaration("", element_ty);
				self.s(") * (");
				self.gen_expression(*index, storage);
				self.s(")))");
			}

			hir::Expression::ManyPointerSlicing { pointer, start, end, element_ty } => {
				self.s("__slice_many_pointer((uint8_t *)(");
				self.gen_expression(*pointer, storage);
				self.s("), (int)sizeof(");
				self.gen_declaration("", element_ty);
				self.s(") * (");
				self.gen_expression(*start, storage);
				self.s("), (int)sizeof(");
				self.gen_declaration("", element_ty);
				self.s(") * (");
				self.gen_expression(*end, storage);
				self.s("))");
			}

			hir::Expression::SliceSlicing { slice, start, end, element_ty } => {
				self.s("__slice_slice(");
				self.gen_expression(*slice, storage);
				self.s(", (int)sizeof(");
				self.gen_declaration("", element_ty);
				self.s(") * (");
				self.gen_expression(*start, storage);
				self.s("), (int)sizeof(");
				self.gen_declaration("", element_ty);
				self.s(") * (");
				self.gen_expression(*end, storage);
				self.s("))");
			}

			hir::Expression::AddressOf(e) => {
				self.s("(&");
				self.gen_expression(*e, storage);
				self.s(")");
			}

			hir::Expression::Dereference(e) => {
				self.s("(*");
				self.gen_expression(*e, storage);
				self.s(")");
			}

			hir::Expression::Cast { ty, operand } => {
				self.s("(");
				self.gen_declaration("", ty);
				self.s(")");
				self.gen_expression(*operand, storage);
			}
		}
	}

	fn gen_declaration(&mut self, name: &str, ty: &resolver::Ty) {
		self.gen_base_ty(ty);
		self.s(" ");
		self.gen_ty_expression(name, ty);
	}

	fn gen_base_ty(&mut self, ty: &resolver::Ty) {
		match ty {
			resolver::Ty::Int => self.s("int"),

			resolver::Ty::Byte => self.s("uint8_t"),

			resolver::Ty::Bool => self.s("bool"),

			resolver::Ty::Named(n) => match &self.index.named_tys[n] {
				resolver::NamedTy::Struct(_) => {
					self.s("struct ");
					self.s(n);
				}
			},

			resolver::Ty::SinglePointer { pointee } => self.gen_base_ty(pointee),

			resolver::Ty::ManyPointer { pointee } => self.gen_base_ty(pointee),

			resolver::Ty::Slice { element: _ } => self.s("struct __slice"),
		}
	}

	fn gen_ty_expression(&mut self, name: &str, ty: &resolver::Ty) {
		match ty {
			resolver::Ty::Int
			| resolver::Ty::Byte
			| resolver::Ty::Bool
			| resolver::Ty::Named(_)
			| resolver::Ty::Slice { element: _ } => self.s(name),
			resolver::Ty::SinglePointer { pointee } | resolver::Ty::ManyPointer { pointee } => {
				self.s("*");
				self.gen_ty_expression(name, pointee);
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
			.arg("-Wno-incompatible-library-redeclaration")
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
