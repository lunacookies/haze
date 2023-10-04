use indexmap::IndexMap;

use crate::{ast, lexer::Loc};

pub fn index(ast: &ast::Ast) -> Index {
	Indexer::default().index(ast)
}

#[derive(Clone, PartialEq, Eq, Default)]
pub struct Index {
	pub procedures: IndexMap<String, Procedure>,
	pub tys: IndexMap<String, TyDefinition>,
}

#[derive(Clone, PartialEq, Eq)]
pub struct Procedure {
	pub parameters: Vec<Parameter>,
	pub return_ty: Option<Ty>,
}

#[derive(Clone, PartialEq, Eq)]
pub struct Parameter {
	pub name: String,
	pub ty: Ty,
}

#[derive(Clone, PartialEq, Eq)]
pub enum TyDefinition {
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
pub struct Ty {
	pub kind: TyKind,
	pub loc: Loc,
}

#[derive(Clone, PartialEq, Eq)]
pub enum TyKind {
	Int,
	Named(String),
}

#[derive(Default)]
struct Indexer {
	index: Index,
}

impl Indexer {
	fn index(mut self, ast: &ast::Ast) -> Index {
		for definition in &ast.definitions {
			self.index_definition(definition);
		}

		self.index
	}

	fn index_definition(&mut self, definition: &ast::Definition) {
		match definition {
			ast::Definition::Procedure(proc) => {
				let mut parameters = Vec::new();

				for parameter in &proc.parameters {
					parameters.push(Parameter {
						name: parameter.name.clone(),
						ty: self.index_ty(&parameter.ty),
					});
				}

				let return_ty = proc.return_ty.as_ref().map(|t| self.index_ty(t));

				self.index
					.procedures
					.insert(proc.name.clone(), Procedure { parameters, return_ty });
			}

			ast::Definition::Struct(strukt) => {
				let mut fields = Vec::new();

				for field in &strukt.fields {
					fields.push(Field { name: field.name.clone(), ty: self.index_ty(&field.ty) });
				}

				self.index.tys.insert(strukt.name.clone(), TyDefinition::Struct(Struct { fields }));
			}
		}
	}

	fn index_ty(&mut self, ty: &ast::Ty) -> Ty {
		let loc = ty.loc.clone();
		match &ty.kind {
			ast::TyKind::Int => Ty { kind: TyKind::Int, loc },
			ast::TyKind::Named(n) => Ty { kind: TyKind::Named(n.clone()), loc },
		}
	}
}
