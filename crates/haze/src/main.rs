use std::{io, path::Path};

use diagnostics::Diagnostics;

fn main() -> io::Result<()> {
	let mut diagnostics = Diagnostics::new();
	let project = project::discover_project(Path::new("example"), &mut diagnostics)?;
	dbg!(project, diagnostics);
	Ok(())
}
