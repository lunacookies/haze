use indexmap::IndexMap;
use la_arena::{ArenaMap, Idx};

use crate::{
	ast,
	hir::{BodyStorage, Expression, Function, Hir, Statement, Variable},
	lexer::Loc,
	resolver::{self, Index, Ty},
};

pub fn analyze(ast: &ast::Ast, index: &Index) -> Hir {
	let mut hir = Hir::default();

	for definition in &ast.definitions {
		match &definition.kind {
			ast::DefinitionKind::Function(func) => match (&func.body, func.is_extern) {
				(Some(body), true) => {
					crate::error(body.loc.clone(), "body provided for extern function".to_string())
				}

				(Some(body), false) => {
					let ctx = SemaContext::new(index, &func.name, body);
					hir.functions.insert(func.name.clone(), ctx.analyze());
				}

				(None, true) => {}

				(None, false) => crate::error(
					definition.loc.clone(),
					"non-extern function has no body".to_string(),
				),
			},
			ast::DefinitionKind::Struct(_) => {}
		}
	}

	hir
}

struct SemaContext<'a> {
	index: &'a Index,
	function: &'a resolver::Function,
	function_body: &'a ast::Statement,
	storage: BodyStorage,
	scopes: Vec<IndexMap<String, Idx<Variable>>>,
	expression_tys: ArenaMap<Idx<Expression>, Ty>,
	loop_nesting_level: usize,
}

impl SemaContext<'_> {
	fn new<'a>(
		index: &'a Index,
		function_name: &str,
		function_body: &'a ast::Statement,
	) -> SemaContext<'a> {
		SemaContext {
			index,
			function: &index.functions[function_name],
			function_body,
			storage: BodyStorage::default(),
			scopes: Vec::new(),
			expression_tys: ArenaMap::new(),
			loop_nesting_level: 0,
		}
	}

	fn analyze(mut self) -> Function {
		self.push_scope();

		for parameter in &self.function.parameters {
			let variable = self.storage.variables.alloc(Variable {
				name: parameter.name.clone(),
				ty: parameter.ty.clone(),
				is_parameter: true,
			});
			self.insert_into_scopes(parameter.name.clone(), variable);
		}

		let body = self.analyze_statement(self.function_body).unwrap();

		Function { storage: self.storage, body }
	}

	fn analyze_statement(&mut self, statement: &ast::Statement) -> Option<Idx<Statement>> {
		let s = match &statement.kind {
			ast::StatementKind::LocalDeclaration { name, ty } => {
				let ty = self.resolve_ty(ty);
				let variable =
					self.alloc_variable(Variable { name: name.clone(), ty, is_parameter: false });

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
				let value = self.analyze_expression(value, None);
				let ty = self.expression_tys[value].clone();
				let variable =
					self.alloc_variable(Variable { name: name.clone(), ty, is_parameter: false });

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
				let condition_idx = self.analyze_expression(condition, Some(&Ty::Bool));
				let condition_ty = &self.expression_tys[condition_idx];

				if condition_ty != &Ty::Bool {
					crate::error(
						condition.loc.clone(),
						format!("can’t use “{condition_ty}” as condition of if statement",),
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

			ast::StatementKind::Loop { initializer, condition, post, body } => {
				self.loop_nesting_level += 1;
				self.push_scope();

				let initializer = initializer.as_ref().map(|initializer| {
					// the initializer can never be a `var` statement
					self.analyze_statement(initializer).unwrap()
				});

				let condition_idx = condition.as_ref().map(|c| {
					let idx = self.analyze_expression(c, Some(&Ty::Bool));
					let ty = &self.expression_tys[idx];

					if ty != &Ty::Bool {
						crate::error(
							c.loc.clone(),
							format!("can’t use “{ty}” as condition of loop"),
						);
					}

					idx
				});

				let post = post.as_ref().and_then(|post| self.analyze_statement(post));

				let empty_body = self.alloc_statement(Statement::Block(Vec::new()));
				let body = self.analyze_statement(body).unwrap_or(empty_body);

				self.pop_scope();
				self.loop_nesting_level -= 1;
				Statement::Loop { initializer, condition: condition_idx, post, body }
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

			ast::StatementKind::Return { value } => match (&self.function.return_ty, value) {
				(Some(return_ty), Some(value)) => {
					let value_idx = self.analyze_expression(value, Some(return_ty));
					let value_ty = &self.expression_tys[value_idx];
					if value_ty != return_ty {
						crate::error(value.loc.clone(), format!("cannot return “{value_ty}” from function which returns “{return_ty}”"));
					}

					Statement::Return { value: Some(value_idx) }
				}

				(Some(return_ty), None) => crate::error(
					statement.loc.clone(),
					format!(
						"cannot return without value from function which returns “{return_ty}”"
					),
				),

				(None, Some(value)) => crate::error(
					value.loc.clone(),
					"cannot return with value from function which does not return value"
						.to_string(),
				),

				(None, None) => Statement::Return { value: None },
			},

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

			ast::StatementKind::Expression(e) => match &e.kind {
				ast::ExpressionKind::Call { name, arguments } => {
					let (arguments, _return_ty) = self.analyze_call(name, arguments, &e.loc);
					Statement::Call { name: name.clone(), arguments }
				}
				_ => Statement::Expression(self.analyze_expression(e, None)),
			},

			ast::StatementKind::Assignment { lhs, rhs } => {
				let lhs_idx = self.analyze_expression(lhs, None);
				let lhs_ty = self.expression_tys[lhs_idx].clone();
				let rhs_idx = self.analyze_expression(rhs, Some(&lhs_ty));
				let rhs_ty = &self.expression_tys[rhs_idx];

				if &lhs_ty != rhs_ty {
					crate::error(
						rhs.loc.clone(),
						format!("can’t assign “{}” to “{}”", rhs_ty, lhs_ty),
					);
				}

				Statement::Assignment { lhs: lhs_idx, rhs: rhs_idx }
			}
		};

		Some(self.alloc_statement(s))
	}

	fn analyze_expression(
		&mut self,
		expression: &ast::Expression,
		requested_ty: Option<&Ty>,
	) -> Idx<Expression> {
		let (e, ty) = match &expression.kind {
			ast::ExpressionKind::Integer(i) => match requested_ty {
				Some(Ty::Byte) => (Expression::Byte(*i as u8), Ty::Byte),
				_ => (Expression::Integer(*i), Ty::Int),
			},

			ast::ExpressionKind::String(s) => {
				(Expression::String(s.clone()), Ty::Slice { element: Box::new(Ty::Byte) })
			}

			ast::ExpressionKind::Character(b) => (Expression::Byte(*b), Ty::Byte),

			ast::ExpressionKind::Variable(name) => match self.lookup_in_scopes(name) {
				Some(variable) => {
					let ty = self.storage.variables[variable].ty.clone();
					(Expression::Variable(variable), ty)
				}
				None => {
					crate::error(expression.loc.clone(), format!("undefined variable “{name}”",))
				}
			},

			ast::ExpressionKind::Call { name, arguments } => {
				let (arguments, return_ty) = self.analyze_call(name, arguments, &expression.loc);

				match return_ty {
					Some(t) => (Expression::Call { name: name.clone(), arguments }, t.clone()),
					None => crate::error(
						expression.loc.clone(),
						format!("cannot call function “{name}” in expression since it does not return a value"),
					),
				}
			}

			ast::ExpressionKind::True => (Expression::True, Ty::Bool),
			ast::ExpressionKind::False => (Expression::False, Ty::Bool),

			ast::ExpressionKind::Binary { lhs, operator, rhs } => {
				let lhs_idx = self.analyze_expression(lhs, None);
				let lhs_ty = self.expression_tys[lhs_idx].clone();
				let rhs_idx = self.analyze_expression(rhs, Some(&lhs_ty));
				let rhs_ty = &self.expression_tys[rhs_idx];

				let acceptable_tys = acceptable_tys_for_binary_operator(*operator);

				if !acceptable_tys.contains(&lhs_ty) {
					crate::error(
						lhs.loc.clone(),
						format!("cannot use operator “{operator}” with “{lhs_ty}”"),
					);
				}

				if !acceptable_tys.contains(rhs_ty) {
					crate::error(
						rhs.loc.clone(),
						format!("cannot use operator “{operator}” with “{rhs_ty}”"),
					);
				}

				if &lhs_ty != rhs_ty {
					crate::error(rhs.loc.clone(), format!("operands must be of the same type, but “{rhs_ty}” is not the same as “{lhs_ty}”"));
				}

				(
					Expression::Binary { lhs: lhs_idx, rhs: rhs_idx, operator: *operator },
					binary_operator_return_ty(*operator, &lhs_ty).clone(),
				)
			}

			ast::ExpressionKind::Unary { operand, operator } => {
				let operand_idx = self.analyze_expression(operand, None);
				let operand_ty = self.expression_tys[operand_idx].clone();

				let acceptable_tys = acceptable_tys_for_unary_operator(*operator);

				if !acceptable_tys.contains(&operand_ty) {
					crate::error(
						operand.loc.clone(),
						format!("cannot use operator “{operator}” with “{operand_ty}”"),
					);
				}

				(
					Expression::Unary { operand: operand_idx, operator: *operator },
					unary_operator_return_ty(*operator, &operand_ty).clone(),
				)
			}

			ast::ExpressionKind::FieldAccess { lhs, field } => {
				let lhs_idx = self.analyze_expression(lhs, None);
				let lhs_ty = &self.expression_tys[lhs_idx];

				match lhs_ty {
					Ty::Int | Ty::Byte | Ty::Bool | Ty::SinglePointer { .. } | Ty::ManyPointer{..} => crate::error(
						lhs.loc.clone(),
						format!("“{lhs_ty}” is not a struct so it has no fields"),
					),

					Ty::Slice { element: element_ty } => match field.as_str() {
						"data" => (
							Expression::SliceData {
								slice: lhs_idx,
								element_ty: element_ty.as_ref().clone(),
							},
							Ty::ManyPointer { pointee: element_ty.clone() },
						),

						"count" => (Expression::SliceCount { slice: lhs_idx, element_ty: element_ty.as_ref().clone()}, Ty::Int),

						_ => crate::error(
							lhs.loc.clone(),
							format!("invalid magic slice field “{field}”: the only valid magic fields are “data” and “count”"),
						),
					},

					Ty::Named(name) => match &self.index.named_tys[name] {
						resolver::NamedTy::Struct(strukt) => 'blk: {
							for f in &strukt.fields {
								if &f.name == field {
									break 'blk (
										Expression::FieldAccess {
											lhs: lhs_idx,
											field: field.clone(),
										},
										f.ty.clone(),
									);
								}
							}

							crate::error(
								lhs.loc.clone(),
								format!("“{lhs_ty}” does not contain a field called “{field}”"),
							)
						}
					},
				}
			}

			ast::ExpressionKind::Indexing { lhs, index } => {
				let lhs_idx = self.analyze_expression(lhs, None);
				let index_idx = self.analyze_expression(index, Some(&Ty::Int));

				self.check_index_ty(index_idx, index.loc.clone());

				match &self.expression_tys[lhs_idx] {
					Ty::ManyPointer { pointee } => (
						Expression::ManyPointerIndexing { lhs: lhs_idx, index: index_idx },
						pointee.as_ref().clone(),
					),

					Ty::Slice { element } => (
						Expression::SliceIndexing {
							slice: lhs_idx,
							index: index_idx,
							element_ty: element.as_ref().clone(),
						},
						element.as_ref().clone(),
					),

					lhs_ty => {
						crate::error(lhs.loc.clone(), format!("cannot index into “{lhs_ty}”"))
					}
				}
			}

			ast::ExpressionKind::Slicing { lhs, start, end } => {
				let lhs_idx = self.analyze_expression(lhs, None);

				let start_idx = match start {
					Some(start) => {
						let i = self.analyze_expression(start, Some(&Ty::Int));
						self.check_index_ty(i, start.loc.clone());
						i
					}

					None => self.alloc_expression(Expression::Integer(0)),
				};

				let end_idx = end.as_ref().map(|end| {
					let i = self.analyze_expression(end, Some(&Ty::Int));
					self.check_index_ty(i, end.loc.clone());
					i
				});

				let lhs_ty = &self.expression_tys[lhs_idx];
				match lhs_ty {
					Ty::ManyPointer { pointee } => match end_idx {
						Some(end_idx) => (
							Expression::ManyPointerSlicing {
								pointer: lhs_idx,
								start: start_idx,
								end: end_idx,
								element_ty: pointee.as_ref().clone(),
							},
							Ty::Slice { element: pointee.clone() },
						),

						None => (
							Expression::ManyPointerOffset { pointer: lhs_idx, offset: start_idx },
							lhs_ty.clone(),
						),
					},

					Ty::Slice { element } => (
						Expression::SliceSlicing {
							slice: lhs_idx,
							start: start_idx,
							end: end_idx,
							element_ty: element.as_ref().clone(),
						},
						lhs_ty.clone(),
					),

					_ => crate::error(lhs.loc.clone(), format!("cannot slice into “{lhs_ty}”")),
				}
			}

			ast::ExpressionKind::AddressOf(e) => {
				let e = self.analyze_expression(e, None);
				let ty = &self.expression_tys[e];

				(Expression::AddressOf(e), Ty::SinglePointer { pointee: Box::new(ty.clone()) })
			}

			ast::ExpressionKind::Dereference(e) => {
				let e_idx = self.analyze_expression(e, None);
				let ty = &self.expression_tys[e_idx];

				let result_ty = match ty {
					Ty::SinglePointer { pointee } => &**pointee,
					_ => crate::error(e.loc.clone(), format!("cannot dereference “{ty}”")),
				};

				(Expression::Dereference(e_idx), result_ty.clone())
			}

			ast::ExpressionKind::Cast { ty, operand } => 'cast: {
				let ty = self.resolve_ty(ty);
				let operand_idx = self.analyze_expression(operand, Some(&ty));
				let operand_ty = &self.expression_tys[operand_idx];

				let lowered_operand = &self.storage.expressions[operand_idx];
				if matches!(lowered_operand, Expression::Integer(_) | Expression::Byte(_))
					&& operand_ty == &ty
				{
					if ty == Ty::Int {
						crate::error(
							expression.loc.clone(),
							"“int” is the default integer type so this cast has no effect"
								.to_string(),
						);
					}

					break 'cast (lowered_operand.clone(), ty);
				}

				if operand_ty == &ty {
					crate::error(
						expression.loc.clone(),
						format!("useless cast from “{operand_ty}” to “{ty}”"),
					);
				}

				match (operand_ty, &ty) {
					(Ty::Int, Ty::Byte) => {}
					(Ty::Byte, Ty::Int) => {}
					(Ty::Bool, Ty::Int) => {}
					(Ty::Bool, Ty::Byte) => {}
					(
						Ty::SinglePointer { .. } | Ty::ManyPointer { .. },
						Ty::SinglePointer { .. } | Ty::ManyPointer { .. },
					) => {}

					_ => crate::error(
						expression.loc.clone(),
						format!("invalid cast from “{operand_ty}” to “{ty}”"),
					),
				}

				(Expression::Cast { ty: ty.clone(), operand: operand_idx }, ty.clone())
			}
		};

		let idx = self.alloc_expression(e);
		self.expression_tys.insert(idx, ty);
		idx
	}

	fn analyze_call(
		&mut self,
		name: &str,
		arguments: &[ast::Expression],
		loc: &Loc,
	) -> (Vec<Idx<Expression>>, Option<&Ty>) {
		let function = match self.index.functions.get(name) {
			Some(p) => p,
			None => crate::error(loc.clone(), format!("undefined function “{name}”")),
		};

		if function.parameters.len() != arguments.len() {
			crate::error(
				loc.clone(),
				format!(
					"“{name}” expected {} arguments but {} were provided",
					function.parameters.len(),
					arguments.len()
				),
			);
		}

		let mut argument_idxs = Vec::new();

		for (argument, parameter) in arguments.iter().zip(&function.parameters) {
			let idx = self.analyze_expression(argument, Some(&parameter.ty));
			let argument_ty = &self.expression_tys[idx];

			if argument_ty != &parameter.ty {
				crate::error(
					argument.loc.clone(),
					format!(
						"“{}” argument was expected but “{argument_ty}” was provided",
						parameter.ty
					),
				);
			}

			argument_idxs.push(idx);
		}

		(argument_idxs, function.return_ty.as_ref())
	}

	fn resolve_ty(&mut self, ty: &ast::Ty) -> Ty {
		match &ty.kind {
			ast::TyKind::Int => Ty::Int,
			ast::TyKind::Byte => Ty::Byte,
			ast::TyKind::Bool => Ty::Bool,

			ast::TyKind::Named(name) => {
				if self.index.named_tys.contains_key(name) {
					Ty::Named(name.clone())
				} else {
					crate::error(ty.loc.clone(), format!("undefined type “{name}”"))
				}
			}

			ast::TyKind::SinglePointer { pointee } => {
				Ty::SinglePointer { pointee: Box::new(self.resolve_ty(pointee)) }
			}

			ast::TyKind::ManyPointer { pointee } => {
				Ty::ManyPointer { pointee: Box::new(self.resolve_ty(pointee)) }
			}

			ast::TyKind::Slice { element } => {
				Ty::Slice { element: Box::new(self.resolve_ty(element)) }
			}
		}
	}

	fn check_index_ty(&self, index: Idx<Expression>, index_loc: Loc) {
		let index_ty = &self.expression_tys[index];
		if index_ty != &Ty::Int {
			crate::error(index_loc, format!("cannot use “{index_ty}” as an index"));
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

fn acceptable_tys_for_binary_operator(operator: ast::BinaryOperator) -> &'static [Ty] {
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
		| ast::BinaryOperator::NotEqual => &[Ty::Int, Ty::Byte],

		ast::BinaryOperator::BitAnd | ast::BinaryOperator::BitOr | ast::BinaryOperator::BitXor => {
			&[Ty::Int, Ty::Byte, Ty::Bool]
		}

		ast::BinaryOperator::And | ast::BinaryOperator::Or => &[Ty::Bool],
	}
}

fn binary_operator_return_ty(operator: ast::BinaryOperator, operand_ty: &Ty) -> &Ty {
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

fn acceptable_tys_for_unary_operator(operator: ast::UnaryOperator) -> &'static [Ty] {
	match operator {
		ast::UnaryOperator::Negate | ast::UnaryOperator::BitNot => &[Ty::Int, Ty::Byte],
		ast::UnaryOperator::Not => &[Ty::Bool],
	}
}

fn unary_operator_return_ty(operator: ast::UnaryOperator, operand_ty: &Ty) -> &Ty {
	match operator {
		ast::UnaryOperator::Negate | ast::UnaryOperator::BitNot => operand_ty,
		ast::UnaryOperator::Not => &Ty::Bool,
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
