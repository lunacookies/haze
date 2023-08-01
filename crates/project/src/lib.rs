#![warn(rust_2018_idioms, unreachable_pub)]

use std::{ffi::OsStr, fs, io, path::Path};

use diagnostics::Diagnostics;

#[derive(Debug)]
pub struct Project {
	pub packages: Vec<Package>,
}

#[derive(Debug)]
pub struct Package {
	pub name: String,
	pub files: Vec<File>,
}

#[derive(Debug)]
pub struct File {
	pub name: String,
	pub content: String,
}

pub fn discover_project(project_path: &Path, diagnostics: &mut Diagnostics) -> io::Result<Project> {
	let mut packages = Vec::new();

	for entry in fs::read_dir(project_path)? {
		let entry = entry?;
		let path = entry.path();

		if let Some(package) = discover_package(&path, diagnostics)? {
			packages.push(package);
		}
	}

	Ok(Project { packages })
}

fn discover_package(
	package_path: &Path,
	diagnostics: &mut Diagnostics,
) -> io::Result<Option<Package>> {
	let Some(name) = package_path.file_name() else {
		return Ok(None);
	};

	let Some(name) = name.to_str() else {
        let message = format!("package at “{package_path:?}” doesn’t have a UTF-8 name");
        diagnostics.add_without_range(message, package_path.to_path_buf());
        return Ok(None);
    };

	let metadata = package_path.metadata()?;
	if !metadata.is_dir() {
		return Ok(None);
	}

	let sources = package_path.join("Sources");
	if !sources.exists() {
		return Ok(None);
	}

	let mut files = Vec::new();

	for entry in fs::read_dir(&sources)? {
		let entry = entry?;
		let path = entry.path();
		let metadata = path.metadata()?;

		if metadata.is_file() && path.extension() == Some(OsStr::new("haze")) {
			let file_stem = path.file_stem().unwrap();
			let Some(name) = file_stem.to_str() else {
                let message = format!("file at “{path:?}” doesn’t have a UTF-8 name");
                diagnostics.add_without_range(message, path.clone());
                continue;
            };
			let content = fs::read_to_string(&path)?;
			files.push(File { name: name.to_string(), content });
		}
	}

	Ok(Some(Package { name: name.to_string(), files }))
}
