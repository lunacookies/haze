use indexmap::IndexMap;
use la_arena::{ArenaMap, Idx};

use crate::{
	ast,
	hir::{BodyStorage, Expression, Hir, Procedure, Statement, Variable},
	resolver::{Index, Ty},
};

pub fn analyze(ast: &ast::Ast, index: &Index) -> Hir {
	let mut hir = Hir::default();

	for definition in &ast.definitions {
		match definition {
			ast::Definition::Procedure(proc) => {
				hir.procedures.insert(proc.name.clone(), SemaContext::new(index).analyze(proc));
			}
			ast::Definition::Struct(_) => {}
		}
	}

	hir
}

struct SemaContext<'a> {
	index: &'a Index,
	storage: BodyStorage,
	scopes: Vec<IndexMap<String, Idx<Variable>>>,
	expression_tys: ArenaMap<Idx<Expression>, Ty>,
	loop_nesting_level: usize,
}

impl SemaContext<'_> {
	fn new(index: &Index) -> SemaContext<'_> {
		SemaContext {
			index,
			storage: BodyStorage::default(),
			scopes: Vec::new(),
			expression_tys: ArenaMap::new(),
			loop_nesting_level: 0,
		}
	}

	fn analyze(mut self, procedure: &ast::Procedure) -> Procedure {
		let indexed_procedure = &self.index.procedures[&procedure.name];
		self.push_scope();

		for parameter in &indexed_procedure.parameters {
			let variable = self
				.storage
				.variables
				.alloc(Variable { name: parameter.name.clone(), ty: parameter.ty.clone() });
			self.insert_into_scopes(parameter.name.clone(), variable);
		}

		let body = self.analyze_statement(&procedure.body).unwrap();

		Procedure { storage: self.storage, body }
	}

	fn analyze_statement(&mut self, statement: &ast::Statement) -> Option<Idx<Statement>> {
		let s = match &statement.kind {
			ast::StatementKind::LocalDeclaration { name, ty } => {
				let ty = self.resolve_ty(ty);
				let variable = self.alloc_variable(Variable { name: name.clone(), ty });

				if self.lookup_in_scopes(name).is_some() {
					crate::error(
						statement.loc.clone(),
						format!("cannot re-declare variable “{name}”"),
					);
				}

				self.insert_into_scopes(name.clone(), variable);

				return None;
			}

			ast::StatementKind::LocalDefinition { name, value } => {
				let value = self.analyze_expression(value);
				let ty = self.expression_tys[value].clone();
				let variable = self.alloc_variable(Variable { name: name.clone(), ty });

				if self.lookup_in_scopes(name).is_some() {
					crate::error(
						statement.loc.clone(),
						format!("cannot re-declare variable “{name}”"),
					);
				}

				self.insert_into_scopes(name.clone(), variable);

				Statement::Assignment {
					lhs: self.alloc_expression(Expression::Variable(variable)),
					rhs: value,
				}
			}

			ast::StatementKind::If { condition, true_branch, false_branch } => {
				let condition_idx = self.analyze_expression(condition);
				let condition_ty = &self.expression_tys[condition_idx];

				if condition_ty != &Ty::Bool {
					crate::error(
						condition.loc.clone(),
						format!(
							"can’t use value of type “{condition_ty}” as condition of if statement",
						),
					);
				}

				let empty_branch = self.alloc_statement(Statement::Block(Vec::new()));
				let true_branch_idx = self.analyze_statement(true_branch).unwrap_or(empty_branch);
				let false_branch_idx = false_branch
					.as_ref()
					.map(|s| self.analyze_statement(s).unwrap_or(empty_branch));

				Statement::If {
					condition: condition_idx,
					true_branch: true_branch_idx,
					false_branch: false_branch_idx,
				}
			}

			ast::StatementKind::Loop { body } => {
				self.loop_nesting_level += 1;

				let empty_body = self.alloc_statement(Statement::Block(Vec::new()));
				let body = self.analyze_statement(body).unwrap_or(empty_body);

				self.loop_nesting_level -= 1;
				Statement::Loop { body }
			}

			ast::StatementKind::Break => {
				if self.loop_nesting_level == 0 {
					crate::error(
						statement.loc.clone(),
						"cannot break while not in a loop".to_string(),
					);
				}

				Statement::Break
			}

			ast::StatementKind::Return { value } => {
				let value = value.as_ref().map(|e| self.analyze_expression(e));
				Statement::Return { value }
			}

			ast::StatementKind::Block(statements) => {
				let mut idxs = Vec::new();
				self.push_scope();

				for child_statement in statements {
					if let Some(s) = self.analyze_statement(child_statement) {
						idxs.push(s);
					}
				}

				self.pop_scope();
				Statement::Block(idxs)
			}

			ast::StatementKind::Expression(e) => Statement::Expression(self.analyze_expression(e)),

			ast::StatementKind::Assignment { lhs, rhs } => {
				let lhs_idx = self.analyze_expression(lhs);
				let rhs_idx = self.analyze_expression(rhs);
				let lhs_ty = &self.expression_tys[lhs_idx];
				let rhs_ty = &self.expression_tys[rhs_idx];

				if lhs_ty != rhs_ty {
					crate::error(
						rhs.loc.clone(),
						format!(
							"can’t assign value of type “{}” to value of type “{}”",
							rhs_ty, lhs_ty
						),
					);
				}

				Statement::Assignment { lhs: lhs_idx, rhs: rhs_idx }
			}
		};

		Some(self.alloc_statement(s))
	}

	fn analyze_expression(&mut self, expression: &ast::Expression) -> Idx<Expression> {
		let (e, ty) = match &expression.kind {
			ast::ExpressionKind::Integer(i) => (Expression::Integer(*i), Ty::Int),

			ast::ExpressionKind::Variable(name) => match self.lookup_in_scopes(name) {
				Some(variable) => {
					let ty = self.storage.variables[variable].ty.clone();
					(Expression::Variable(variable), ty)
				}
				None => {
					crate::error(expression.loc.clone(), format!("undefined variable “{name}”",))
				}
			},

			ast::ExpressionKind::True => (Expression::True, Ty::Bool),
			ast::ExpressionKind::False => (Expression::False, Ty::Bool),

			ast::ExpressionKind::Binary { lhs, operator, rhs } => {
				let lhs_idx = self.analyze_expression(lhs);
				let rhs_idx = self.analyze_expression(rhs);
				let lhs_ty = &self.expression_tys[lhs_idx];
				let rhs_ty = &self.expression_tys[rhs_idx];

				let acceptable_tys = acceptable_tys_for_operator(*operator);

				if !acceptable_tys.contains(lhs_ty) {
					crate::error(
						lhs.loc.clone(),
						format!("cannot use operator “{operator}” with value of type “{lhs_ty}”"),
					);
				}

				if !acceptable_tys.contains(rhs_ty) {
					crate::error(
						rhs.loc.clone(),
						format!("cannot use operator “{operator}” with value of type “{rhs_ty}”"),
					);
				}

				assert_eq!(lhs_ty, rhs_ty);

				(
					Expression::Binary { lhs: lhs_idx, rhs: rhs_idx, op: *operator },
					operator_return_ty(*operator, lhs_ty).clone(),
				)
			}
		};

		let idx = self.alloc_expression(e);
		self.expression_tys.insert(idx, ty);
		idx
	}

	fn resolve_ty(&mut self, ty: &ast::Ty) -> Ty {
		match &ty.kind {
			ast::TyKind::Int => Ty::Int,
			ast::TyKind::Bool => Ty::Bool,
			ast::TyKind::Named(name) => {
				if self.index.named_tys.contains_key(name) {
					Ty::Named(name.clone())
				} else {
					crate::error(ty.loc.clone(), format!("undefined type “{name}”"))
				}
			}
		}
	}

	fn alloc_variable(&mut self, variable: Variable) -> Idx<Variable> {
		self.storage.variables.alloc(variable)
	}

	fn alloc_statement(&mut self, statement: Statement) -> Idx<Statement> {
		self.storage.statements.alloc(statement)
	}

	fn alloc_expression(&mut self, expression: Expression) -> Idx<Expression> {
		self.storage.expressions.alloc(expression)
	}

	fn lookup_in_scopes(&mut self, name: &str) -> Option<Idx<Variable>> {
		for scope in self.scopes.iter().rev() {
			if let Some(variable) = scope.get(name).copied() {
				return Some(variable);
			}
		}

		None
	}

	fn insert_into_scopes(&mut self, name: String, variable: Idx<Variable>) {
		let current_scope = &mut self.scopes.last_mut().unwrap();
		assert!(!current_scope.contains_key(&name));
		current_scope.insert(name, variable);
	}

	fn push_scope(&mut self) {
		self.scopes.push(IndexMap::new());
	}

	fn pop_scope(&mut self) {
		self.scopes.pop();
	}
}

fn acceptable_tys_for_operator(operator: ast::BinaryOperator) -> &'static [Ty] {
	match operator {
		ast::BinaryOperator::Add
		| ast::BinaryOperator::Subtract
		| ast::BinaryOperator::Multiply
		| ast::BinaryOperator::Divide
		| ast::BinaryOperator::Modulo
		| ast::BinaryOperator::ShiftLeft
		| ast::BinaryOperator::ShiftRight
		| ast::BinaryOperator::Less
		| ast::BinaryOperator::Greater
		| ast::BinaryOperator::LessEqual
		| ast::BinaryOperator::GreaterEqual
		| ast::BinaryOperator::Equal
		| ast::BinaryOperator::NotEqual => &[Ty::Int],

		ast::BinaryOperator::BitAnd | ast::BinaryOperator::BitOr | ast::BinaryOperator::BitXor => {
			&[Ty::Int, Ty::Bool]
		}

		ast::BinaryOperator::And | ast::BinaryOperator::Or => &[Ty::Bool],
	}
}

fn operator_return_ty(operator: ast::BinaryOperator, operand_ty: &Ty) -> &Ty {
	match operator {
		ast::BinaryOperator::Add
		| ast::BinaryOperator::Subtract
		| ast::BinaryOperator::Multiply
		| ast::BinaryOperator::Divide
		| ast::BinaryOperator::Modulo
		| ast::BinaryOperator::ShiftLeft
		| ast::BinaryOperator::ShiftRight
		| ast::BinaryOperator::BitAnd
		| ast::BinaryOperator::BitOr
		| ast::BinaryOperator::BitXor => operand_ty,

		ast::BinaryOperator::And | ast::BinaryOperator::Or => {
			assert_eq!(operand_ty, &Ty::Bool);
			&Ty::Bool
		}

		ast::BinaryOperator::Equal
		| ast::BinaryOperator::NotEqual
		| ast::BinaryOperator::Less
		| ast::BinaryOperator::Greater
		| ast::BinaryOperator::LessEqual
		| ast::BinaryOperator::GreaterEqual => &Ty::Bool,
	}
}

#[cfg(test)]
#[test]
fn tests() {
	use std::path::PathBuf;

	crate::testing::run_tests("test_data_sema", |input| {
		let ast = crate::parser::parse(input, PathBuf::from("test"));
		let index = crate::indexer::index(&ast);
		let index = crate::resolver::resolve(&index);
		let hir = analyze(&ast, &index);
		hir.pretty_print()
	});
}
