use std::fmt;

use indexmap::IndexMap;

use crate::indexer;

pub fn resolve(index: &indexer::Index) -> Index {
	Resolver::new(index).resolve()
}

#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub struct Index {
	pub procedures: IndexMap<String, Procedure>,
	pub named_tys: IndexMap<String, NamedTy>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Procedure {
	pub parameters: Vec<Parameter>,
	pub return_ty: Option<Ty>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Parameter {
	pub name: String,
	pub ty: Ty,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum NamedTy {
	Struct(Struct),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Struct {
	pub fields: Vec<Field>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Field {
	pub name: String,
	pub ty: Ty,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Ty {
	Int,
	Bool,
	Named(String),
}

struct Resolver<'a> {
	index: &'a indexer::Index,
	procedures: IndexMap<String, Procedure>,
	named_tys: IndexMap<String, NamedTy>,
	in_progress_tys: Vec<String>,
}

impl Resolver<'_> {
	fn new(index: &indexer::Index) -> Resolver<'_> {
		Resolver {
			index,
			procedures: IndexMap::new(),
			named_tys: IndexMap::new(),
			in_progress_tys: Vec::new(),
		}
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
			indexer::TyKind::Bool => Ty::Bool,
			indexer::TyKind::Named(n) => {
				let n = n.clone();

				if !self.named_tys.contains_key(&n) {
					if self.in_progress_tys.contains(&n) {
						crate::error(
							ty.loc.clone(),
							"detected cycle in type definition".to_string(),
						);
					}

					self.in_progress_tys.push(n.clone());
					let in_progress_len = self.in_progress_tys.len();

					let definition = match self.index.tys.get(&n) {
						Some(d) => d,
						None => crate::error(ty.loc.clone(), "undefined type".to_string()),
					};
					self.resolve_ty_definition(&n, definition);

					assert_eq!(self.in_progress_tys.len(), in_progress_len);
					self.in_progress_tys.pop();
				}

				Ty::Named(n)
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
				s.push_str(&parameter.ty.to_string());
			}

			s.push(')');

			if let Some(return_ty) = &procedure.return_ty {
				s.push(' ');
				s.push_str(&return_ty.to_string())
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
						s.push_str(&field.ty.to_string());
					}

					s.push_str("\n}\n");
				}
			}
		}

		s
	}
}

impl fmt::Display for Ty {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		match self {
			Ty::Int => write!(f, "int"),
			Ty::Bool => write!(f, "bool"),
			Ty::Named(name) => write!(f, "{name}"),
		}
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
