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

	p.expect_with_name(TokenKind::Identifier, "function name");

	if p.at(TokenKind::OpenParenthesis) {
		parameter_list(p);
	} else {
		p.error("parameter list");
	}

	if p.eat(TokenKind::Arrow) {
		ty(p);
	}

	p.expect(TokenKind::OpenBrace);
	p.expect(TokenKind::CloseBrace);

	m.complete(p, NodeKind::Function)
}

fn parameter_list(p: &mut Parser<'_>) -> CompletedMarker {
	assert!(p.at(TokenKind::OpenParenthesis));
	let m = p.start();
	p.bump(TokenKind::OpenParenthesis);

	while !p.at(TokenKind::CloseParenthesis) {
		let m = p.start();
		p.expect_with_name(TokenKind::Identifier, "parameter name");
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
