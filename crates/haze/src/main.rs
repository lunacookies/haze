use std::{io, path::Path, sync::Arc};

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
				println!("{}", project.file(file_id).name);
			});
		}
	}

	Ok(())
}
