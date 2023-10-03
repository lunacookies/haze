use crate::lexer::Loc;

#[derive(Clone, PartialEq, Eq)]
pub struct Ast {
	pub definitions: Vec<Definition>,
}

#[derive(Clone, PartialEq, Eq)]
pub enum Definition {
	Procedure(Procedure),
}

#[derive(Clone, PartialEq, Eq)]
pub struct Procedure {
	pub name: String,
	pub parameters: Vec<Parameter>,
	pub return_ty: Option<Ty>,
	pub body: Statement,
}

#[derive(Clone, PartialEq, Eq)]
pub struct Parameter {
	pub name: String,
	pub ty: Ty,
}

#[derive(Clone, PartialEq, Eq)]
pub struct Statement {
	pub kind: StatementKind,
	pub loc: Loc,
}

#[derive(Clone, PartialEq, Eq)]
pub enum StatementKind {
	LocalDeclaration { name: String, ty: Ty },
	LocalDefinition { name: String, value: Expression },
	Return { value: Option<Expression> },
	Block(Vec<Statement>),
	Expression(Expression),
	Assignment { lhs: Expression, rhs: Expression },
}

#[derive(Clone, PartialEq, Eq)]
pub struct Expression {
	pub kind: ExpressionKind,
	pub loc: Loc,
}

#[derive(Clone, PartialEq, Eq)]
pub enum ExpressionKind {
	Integer(u64),
	Variable(String),
	True,
	False,
	Binary { lhs: Box<Expression>, operator: BinaryOperator, rhs: Box<Expression> },
}

#[derive(Clone, Copy, PartialEq, Eq)]
pub enum BinaryOperator {
	Add,
	Subtract,
	Multiply,
	Divide,
	Modulo,
	ShiftLeft,
	ShiftRight,
	BitAnd,
	BitOr,
	BitXor,
	And,
	Or,
	Equal,
	NotEqual,
	Less,
	Greater,
	LessEqual,
	GreaterEqual,
}

#[derive(Clone, PartialEq, Eq)]
pub enum Ty {
	Int,
}
