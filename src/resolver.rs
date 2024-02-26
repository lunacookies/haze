use std::fmt;

use indexmap::IndexMap;

use crate::indexer;

pub fn resolve(index: &indexer::Index) -> Index {
	Resolver::new(index).resolve()
}

#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub struct Index {
	pub functions: IndexMap<String, Function>,
	pub named_tys: IndexMap<String, NamedTy>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Function {
	pub parameters: Vec<Parameter>,
	pub return_ty: Option<Ty>,
	pub is_extern: bool,
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
	Byte,
	Bool,
	Named(String),
	Pointer { pointee: Box<Ty> },
	Slice { element: Box<Ty> },
}

struct Resolver<'a> {
	index: &'a indexer::Index,
	functions: IndexMap<String, Function>,
	named_tys: IndexMap<String, NamedTy>,
	in_progress_tys: Vec<String>,
}

impl Resolver<'_> {
	fn new(index: &indexer::Index) -> Resolver<'_> {
		Resolver {
			index,
			functions: IndexMap::new(),
			named_tys: IndexMap::new(),
			in_progress_tys: Vec::new(),
		}
	}

	fn resolve(mut self) -> Index {
		for (name, ty) in &self.index.tys {
			self.resolve_ty_definition(name, ty);
		}

		for (name, function) in &self.index.functions {
			self.resolve_function(name, function);
		}

		Index { functions: self.functions, named_tys: self.named_tys }
	}

	fn resolve_function(&mut self, name: &str, function: &indexer::Function) {
		let mut parameters = Vec::new();

		for parameter in &function.parameters {
			parameters.push(Parameter {
				name: parameter.name.clone(),
				ty: self.resolve_ty(&parameter.ty),
			});
		}

		let return_ty = function.return_ty.as_ref().map(|t| self.resolve_ty(t));

		self.functions.insert(
			name.to_string(),
			Function { parameters, return_ty, is_extern: function.is_extern },
		);
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

			indexer::TyKind::Byte => Ty::Byte,

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

			indexer::TyKind::Pointer { pointee } => {
				Ty::Pointer { pointee: Box::new(self.resolve_ty(pointee)) }
			}

			indexer::TyKind::Slice { element } => {
				Ty::Slice { element: Box::new(self.resolve_ty(element)) }
			}
		}
	}
}

impl Index {
	pub fn pretty_print(&self) -> String {
		let mut s = String::new();

		for (name, function) in &self.functions {
			if !s.is_empty() {
				s.push('\n');
			}

			s.push_str("func ");
			s.push_str(name);
			s.push('(');

			for (i, parameter) in function.parameters.iter().enumerate() {
				if i != 0 {
					s.push_str(", ");
				}

				s.push_str(&parameter.name);
				s.push(' ');
				s.push_str(&parameter.ty.to_string());
			}

			s.push(')');

			if let Some(return_ty) = &function.return_ty {
				s.push(' ');
				s.push_str(&return_ty.to_string());
			}

			if function.is_extern {
				s.push_str(" #extern");
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
			Ty::Byte => write!(f, "byte"),
			Ty::Bool => write!(f, "bool"),
			Ty::Named(name) => write!(f, "{name}"),
			Ty::Pointer { pointee } => write!(f, "*{pointee}"),
			Ty::Slice { element } => write!(f, "[]{element}"),
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
