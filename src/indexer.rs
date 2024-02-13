use indexmap::IndexMap;

use crate::{ast, lexer::Loc};

pub fn index(ast: &ast::Ast) -> Index {
	Indexer::default().index(ast)
}

#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub struct Index {
	pub functions: IndexMap<String, Function>,
	pub tys: IndexMap<String, TyDefinition>,
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
pub enum TyDefinition {
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
pub struct Ty {
	pub kind: TyKind,
	pub loc: Loc,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TyKind {
	Int,
	Byte,
	Bool,
	Named(String),
	Pointer { pointee: Box<Ty> },
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
		match &definition.kind {
			ast::DefinitionKind::Function(func) => {
				let mut parameters = Vec::new();

				for parameter in &func.parameters {
					parameters.push(Parameter {
						name: parameter.name.clone(),
						ty: Self::index_ty(&parameter.ty),
					});
				}

				let return_ty = func.return_ty.as_ref().map(Self::index_ty);

				self.index.functions.insert(
					func.name.clone(),
					Function { parameters, return_ty, is_extern: func.is_extern },
				);
			}

			ast::DefinitionKind::Struct(strukt) => {
				let mut fields = Vec::new();

				for field in &strukt.fields {
					fields.push(Field { name: field.name.clone(), ty: Self::index_ty(&field.ty) });
				}

				self.index.tys.insert(strukt.name.clone(), TyDefinition::Struct(Struct { fields }));
			}
		}
	}

	fn index_ty(ty: &ast::Ty) -> Ty {
		let loc = ty.loc.clone();
		match &ty.kind {
			ast::TyKind::Int => Ty { kind: TyKind::Int, loc },
			ast::TyKind::Byte => Ty { kind: TyKind::Byte, loc },
			ast::TyKind::Bool => Ty { kind: TyKind::Bool, loc },
			ast::TyKind::Named(n) => Ty { kind: TyKind::Named(n.clone()), loc },
			ast::TyKind::Pointer { pointee } => {
				Ty { kind: TyKind::Pointer { pointee: Box::new(Self::index_ty(pointee)) }, loc }
			}
		}
	}
}
