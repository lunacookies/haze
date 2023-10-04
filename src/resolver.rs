use indexmap::IndexMap;

use crate::indexer;

pub fn resolve(index: &indexer::Index) -> Index {
	Resolver::new(index).resolve()
}

#[derive(Clone, PartialEq, Eq, Default)]
pub struct Index {
	pub procedures: IndexMap<String, Procedure>,
	pub named_tys: IndexMap<String, NamedTy>,
}

#[derive(Clone, PartialEq, Eq)]
pub struct Procedure {
	parameters: Vec<Parameter>,
	return_ty: Option<Ty>,
}

#[derive(Clone, PartialEq, Eq)]
pub struct Parameter {
	pub name: String,
	pub ty: Ty,
}

#[derive(Clone, PartialEq, Eq)]
pub enum NamedTy {
	Struct(Struct),
}

#[derive(Clone, PartialEq, Eq)]
pub struct Struct {
	pub fields: Vec<Field>,
}

#[derive(Clone, PartialEq, Eq)]
pub struct Field {
	pub name: String,
	pub ty: Ty,
}

#[derive(Clone, PartialEq, Eq)]
pub enum Ty {
	Int,
	Named(String),
}

struct Resolver<'a> {
	index: &'a indexer::Index,
	procedures: IndexMap<String, Procedure>,
	named_tys: IndexMap<String, NamedTy>,
}

impl Resolver<'_> {
	fn new(index: &indexer::Index) -> Resolver<'_> {
		Resolver { index, procedures: IndexMap::new(), named_tys: IndexMap::new() }
	}

	fn resolve(mut self) -> Index {
		for (name, ty) in &self.index.tys {
			self.resolve_ty_definition(name, ty);
		}

		for (name, procedure) in &self.index.procedures {
			self.resolve_procedure(name, procedure);
		}

		Index { procedures: self.procedures, named_tys: self.named_tys }
	}

	fn resolve_procedure(&mut self, name: &str, procedure: &indexer::Procedure) {
		let mut parameters = Vec::new();

		for parameter in &procedure.parameters {
			parameters.push(Parameter {
				name: parameter.name.clone(),
				ty: self.resolve_ty(&parameter.ty),
			});
		}

		let return_ty = procedure.return_ty.as_ref().map(|t| self.resolve_ty(t));

		self.procedures.insert(name.to_string(), Procedure { parameters, return_ty });
	}

	fn resolve_ty_definition(&mut self, name: &str, ty_definition: &indexer::TyDefinition) {
		match ty_definition {
			indexer::TyDefinition::Struct(strukt) => {
				let mut fields = Vec::new();

				for field in &strukt.fields {
					fields.push(Field { name: field.name.clone(), ty: self.resolve_ty(&field.ty) });
				}

				self.named_tys.insert(name.to_string(), NamedTy::Struct(Struct { fields }));
			}
		}
	}

	fn resolve_ty(&mut self, ty: &indexer::Ty) -> Ty {
		match &ty.kind {
			indexer::TyKind::Int => Ty::Int,
			indexer::TyKind::Named(n) => {
				if !self.named_tys.contains_key(n) {
					let definition = match self.index.tys.get(n) {
						Some(d) => d,
						None => crate::error(ty.loc.clone(), "undefined type".to_string()),
					};
					self.resolve_ty_definition(n, definition);
				}

				Ty::Named(n.clone())
			}
		}
	}
}

impl Index {
	pub fn pretty_print(&self) -> String {
		let mut s = String::new();

		for (name, procedure) in &self.procedures {
			if !s.is_empty() {
				s.push('\n');
			}

			s.push_str("proc ");
			s.push_str(name);
			s.push('(');

			for (i, parameter) in procedure.parameters.iter().enumerate() {
				if i != 0 {
					s.push_str(", ");
				}

				s.push_str(&parameter.name);
				s.push(' ');
				pretty_print_ty(&parameter.ty, &mut s);
			}

			s.push(')');

			if let Some(return_ty) = &procedure.return_ty {
				s.push(' ');
				pretty_print_ty(return_ty, &mut s);
			}

			s.push('\n');
		}

		for (name, ty) in &self.named_tys {
			if !s.is_empty() {
				s.push('\n');
			}

			match ty {
				NamedTy::Struct(strukt) => {
					s.push_str("struct ");
					s.push_str(name);
					s.push_str("\n{");

					for field in &strukt.fields {
						s.push_str("\n\t");
						s.push_str(&field.name);
						s.push(' ');
						pretty_print_ty(&field.ty, &mut s);
					}

					s.push_str("\n}\n");
				}
			}
		}

		s
	}
}

fn pretty_print_ty(ty: &Ty, s: &mut String) {
	match ty {
		Ty::Int => s.push_str("int"),
		Ty::Named(n) => s.push_str(n),
	}
}

#[cfg(test)]
#[test]
fn tests() {
	use std::path::PathBuf;

	crate::testing::run_tests("test_data_resolver", |input| {
		let ast = crate::parser::parse(input, PathBuf::from("test"));
		let index = indexer::index(&ast);
		let resolved = resolve(&index);
		resolved.pretty_print()
	});
}
