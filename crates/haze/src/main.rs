use std::{
	io::{self, Write},
	path::Path,
	sync::Arc,
};

use diagnostics::Diagnostics;

fn main() -> io::Result<()> {
	thread::set_qos_class(thread::QosClass::UserInitiated);

	let mut diagnostics = Diagnostics::new();
	let project = project::discover_project(Path::new("example"), &mut diagnostics)?;
	let project = Arc::new(project);

	thread::set_qos_class(thread::QosClass::Utility);

	let pool = thread::Pool::new();
	for package in &project.packages {
		for &file_id in &package.files {
			let project = Arc::clone(&project);
			pool.submit_job(move || {
				let file = project.file(file_id);
				let tokens = lexer::lex(&file.content);
				let mut stdout = io::stdout().lock();
				writeln!(stdout, "{}:\n{}", file.name, tokens.debug(&file.content)).unwrap();
			});
		}
	}

	Ok(())
}
