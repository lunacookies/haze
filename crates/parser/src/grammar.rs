use syntax::NodeKind;

use crate::{CompletedMarker, Parser};

pub(crate) fn source_file(p: &mut Parser<'_>) -> CompletedMarker {
	let m = p.start();

	while !p.at_eof() {
		p.advance_with_error("expected item");
	}

	m.complete(p, NodeKind::SourceFile)
}
