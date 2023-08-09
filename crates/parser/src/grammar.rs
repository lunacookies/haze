use syntax::NodeKind;

use crate::{CompletedMarker, Parser};

pub(crate) fn source_file(p: &mut Parser<'_>) -> CompletedMarker {
	let m = p.start();
	m.complete(p, NodeKind::SourceFile)
}
