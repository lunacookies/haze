#![warn(rust_2018_idioms, unreachable_pub)]

use std::{ffi::OsStr, fs, io, path::Path};

use diagnostics::Diagnostics;

#[derive(Debug)]
pub struct Project {
	pub packages: Vec<Package>,
	pub files: Vec<File>,
}

#[derive(Debug)]
pub struct Package {
	pub name: String,
	pub files: Vec<FileId>,
}

#[derive(Debug)]
pub struct File {
	pub name: String,
	pub content: String,
}

#[derive(Debug, Clone, Copy)]
pub struct FileId {
	inner: u32,
}

impl Project {
	pub fn file(&self, file_id: FileId) -> &File {
		&self.files[file_id.inner as usize]
	}
}

pub fn discover_project(project_path: &Path, diagnostics: &mut Diagnostics) -> io::Result<Project> {
	ProjectDiscoveryContext {
		project: Project { packages: Vec::new(), files: Vec::new() },
		diagnostics,
		next_file_id: FileId { inner: 0 },
	}
	.discover(project_path)
}

struct ProjectDiscoveryContext<'a> {
	project: Project,
	diagnostics: &'a mut Diagnostics,
	next_file_id: FileId,
}

impl ProjectDiscoveryContext<'_> {
	fn discover(mut self, project_path: &Path) -> io::Result<Project> {
		for entry in fs::read_dir(project_path)? {
			let entry = entry?;
			let path = entry.path();

			if let Some(package) = self.discover_package(&path)? {
				self.project.packages.push(package);
			}
		}

		Ok(self.project)
	}

	fn discover_package(&mut self, package_path: &Path) -> io::Result<Option<Package>> {
		let Some(name) = package_path.file_name() else {
			return Ok(None);
		};

		let Some(name) = name.to_str() else {
			let message = format!("package at “{package_path:?}” doesn’t have a UTF-8 name");
			self. diagnostics.add_without_range(message, package_path.to_path_buf());
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
					self.diagnostics.add_without_range(message, path.clone());
					continue;
				};
				let content = fs::read_to_string(&path)?;
				let file = File { name: name.to_string(), content };
				let file_id = self.allocate_file(file);
				files.push(file_id);
			}
		}

		Ok(Some(Package { name: name.to_string(), files }))
	}

	fn allocate_file(&mut self, file: File) -> FileId {
		let id = self.next_file_id;
		self.next_file_id.inner += 1;
		self.project.files.push(file);
		id
	}
}
