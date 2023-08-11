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

	if !p.at(TokenKind::OpenBrace) {
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

fn statement(p: &mut Parser<'_>) -> CompletedMarker {
	match p.current() {
		TokenKind::Identifier if p.at_nth(1, TokenKind::ColonEquals) => {
			let m = p.start();
			p.bump(TokenKind::Identifier);
			p.bump(TokenKind::ColonEquals);

			expression(p);

			m.complete(p, NodeKind::VariableDeclaration)
		}

		_ => {
			let m = p.start();
			expression(p);
			m.complete(p, NodeKind::ExpressionStatement)
		}
	}
}

fn expression(p: &mut Parser<'_>) -> Option<CompletedMarker> {
	match p.current() {
		TokenKind::Identifier => {
			if p.nth(1) == TokenKind::OpenParenthesis {
				return Some(call(p));
			}

			let m = p.start();
			p.bump(TokenKind::Identifier);
			Some(m.complete(p, NodeKind::PathExpression))
		}

		TokenKind::Number => {
			let m = p.start();
			p.bump(TokenKind::Number);
			Some(m.complete(p, NodeKind::IntegerLiteral))
		}

		TokenKind::IfKw => Some(if_expression(p)),

		_ => p.advance_with_error("expected expression"),
	}
}

fn call(p: &mut Parser<'_>) -> CompletedMarker {
	assert!(p.at(TokenKind::Identifier));
	assert!(p.at_nth(1, TokenKind::OpenParenthesis));
	let m = p.start();
	p.bump(TokenKind::Identifier);
	p.bump(TokenKind::OpenParenthesis);

	while !p.at_eof() && !p.at(TokenKind::CloseParenthesis) {
		let m = p.start();

		if p.at(TokenKind::Identifier) && p.at_nth(1, TokenKind::Colon) {
			let m = p.start();
			p.bump(TokenKind::Identifier);
			p.bump(TokenKind::Colon);
			m.complete(p, NodeKind::ArgumentLabel);
		}

		expression(p);

		if !p.at(TokenKind::CloseParenthesis) {
			p.expect(TokenKind::Comma);
		}

		m.complete(p, NodeKind::Argument);
	}

	p.expect(TokenKind::CloseParenthesis);

	m.complete(p, NodeKind::Call)
}

fn if_expression(p: &mut Parser<'_>) -> CompletedMarker {
	assert!(p.at(TokenKind::IfKw));
	let m = p.start();
	p.bump(TokenKind::IfKw);

	expression(p);
	block_expression_opt(p);

	if p.eat(TokenKind::ElseKw) {
		block_expression_opt(p);
	}

	m.complete(p, NodeKind::IfExpression)
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
