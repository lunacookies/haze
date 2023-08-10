use syntax::{NodeKind, TokenKind};

use crate::{CompletedMarker, Parser};

pub(crate) fn source_file(p: &mut Parser<'_>) -> CompletedMarker {
	let m = p.start();

	while !p.at_eof() {
		match p.current() {
			TokenKind::FuncKw => function(p),
			_ => {
				p.advance_with_error("expected item");
				continue;
			}
		};
	}

	m.complete(p, NodeKind::SourceFile)
}

fn function(p: &mut Parser<'_>) -> CompletedMarker {
	assert!(p.at(TokenKind::FuncKw));
	let m = p.start();
	p.bump(TokenKind::FuncKw);

	p.expect_with_name(TokenKind::Identifier, "expected function name");

	if p.at(TokenKind::OpenParenthesis) {
		parameter_list(p);
	} else {
		p.error("expected parameter list");
	}

	if p.eat(TokenKind::Arrow) {
		ty(p);
	}

	block_expression_opt(p);

	m.complete(p, NodeKind::Function)
}

fn parameter_list(p: &mut Parser<'_>) -> CompletedMarker {
	assert!(p.at(TokenKind::OpenParenthesis));
	let m = p.start();
	p.bump(TokenKind::OpenParenthesis);

	while !p.at_eof() && !p.at(TokenKind::CloseParenthesis) {
		let m = p.start();
		p.expect_with_name(TokenKind::Identifier, "expected parameter name");
		p.expect(TokenKind::Colon);
		ty(p);
		m.complete(p, NodeKind::Parameter);

		if !p.at(TokenKind::CloseParenthesis) {
			p.expect(TokenKind::Comma);
		}
	}

	p.expect(TokenKind::CloseParenthesis);

	m.complete(p, NodeKind::ParameterList)
}

fn ty(p: &mut Parser<'_>) -> CompletedMarker {
	let m = p.start();
	p.expect(TokenKind::Identifier);
	m.complete(p, NodeKind::Type)
}

fn statement(p: &mut Parser<'_>) -> Option<CompletedMarker> {
	match p.current() {
		TokenKind::Identifier => {
			let m = p.start();
			p.bump(TokenKind::Identifier);

			p.expect(TokenKind::ColonEquals);
			expression(p);

			Some(m.complete(p, NodeKind::VariableDeclaration))
		}

		_ => p.advance_with_error("expected statement"),
	}
}

fn expression(p: &mut Parser<'_>) -> Option<CompletedMarker> {
	match p.current() {
		TokenKind::Identifier => {
			let m = p.start();
			p.bump(TokenKind::Identifier);
			Some(m.complete(p, NodeKind::PathExpression))
		}

		TokenKind::Number => {
			let m = p.start();
			p.bump(TokenKind::Number);
			Some(m.complete(p, NodeKind::IntegerLiteral))
		}

		_ => p.advance_with_error("expected expression"),
	}
}

fn block_expression_opt(p: &mut Parser<'_>) -> Option<CompletedMarker> {
	if p.at(TokenKind::OpenBrace) {
		return Some(block_expression(p));
	}

	p.advance_with_error("expected block")
}

fn block_expression(p: &mut Parser<'_>) -> CompletedMarker {
	assert!(p.at(TokenKind::OpenBrace));
	let m = p.start();
	p.bump(TokenKind::OpenBrace);

	while !p.at_eof() && !p.at(TokenKind::CloseBrace) {
		statement(p);
	}
	p.expect(TokenKind::CloseBrace);

	m.complete(p, NodeKind::BlockExpression)
}
