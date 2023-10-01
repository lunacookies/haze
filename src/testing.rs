use std::{fs, path::PathBuf};

const DELIMITER: &str = "===\n";

pub fn run_tests(path: &str, f: impl Fn(&str) -> String) {
	let dir = PathBuf::from(path);

	for entry in fs::read_dir(dir).unwrap() {
		let entry = entry.unwrap();
		let path = entry.path().canonicalize().unwrap();
		let content = fs::read_to_string(&path).unwrap();

		let (input, _expected_output) = content.split_once(DELIMITER).unwrap();
		let actual_output = f(input);
		let actual_content = format!("{input}{DELIMITER}{actual_output}");

		expect_test::expect_file![&path].assert_eq(&actual_content);
	}
}
