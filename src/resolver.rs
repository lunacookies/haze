use std::collections::HashMap;

use crate::indexer;

pub fn resolve(index: &indexer::Index) -> Index {
	Resolver::default().resolve(index)
}

#[derive(Clone, PartialEq, Eq, Default)]
pub struct Index {
	pub procedures: HashMap<String, Procedure>,
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
pub enum Ty {
	Int,
}

#[derive(Default)]
struct Resolver {
	procedures: HashMap<String, Procedure>,
}

impl Resolver {
	fn resolve(mut self, index: &indexer::Index) -> Index {
		for (name, procedure) in &index.procedures {
			self.resolve_procedure(name, procedure);
		}

		Index { procedures: self.procedures }
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

	fn resolve_ty(&mut self, ty: &indexer::Ty) -> Ty {
		match ty {
			indexer::Ty::Int => Ty::Int,
		}
	}
}

impl Index {
	pub fn pretty_print(&self) -> String {
		let mut s = String::new();

		let mut procedures: Vec<_> = self.procedures.iter().collect();
		procedures.sort_by_key(|(name, _)| *name);

		for (name, procedure) in procedures {
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

		s
	}
}

fn pretty_print_ty(ty: &Ty, s: &mut String) {
	match ty {
		Ty::Int => s.push_str("int"),
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
