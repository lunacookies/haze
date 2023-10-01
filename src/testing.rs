use std::{fs, path::PathBuf};

const DELIMITER: &str = "======\n";

pub fn run_tests(path: &str, f: impl Fn(&str) -> String + std::panic::RefUnwindSafe) {
	let dir = PathBuf::from(path);

	for entry in fs::read_dir(dir).unwrap() {
		let entry = entry.unwrap();
		let path = entry.path().canonicalize().unwrap();
		let content = fs::read_to_string(&path).unwrap();

		let (input, _expected_output) = content.split_once(DELIMITER).unwrap();
		let actual_output = match std::panic::catch_unwind(|| f(input)) {
			Ok(s) => s,
			Err(e) => format!("{}", e.downcast::<String>().unwrap()),
		};
		let actual_content = format!("{input}{DELIMITER}{actual_output}");

		expect_test::expect_file![&path].assert_eq(&actual_content);
	}
}
