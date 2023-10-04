use std::{fs, path::PathBuf};

const DELIMITER: &str = "======\n";

pub fn run_tests(path: &str, f: impl Fn(&str) -> String + std::panic::RefUnwindSafe) {
	let dir = PathBuf::from(path);

	for entry in fs::read_dir(dir).unwrap() {
		let entry = entry.unwrap();
		let path = entry.path().canonicalize().unwrap();
		let content = fs::read_to_string(&path).unwrap();

		println!("=== RUNNING TEST: {}", path.display());

		let (input, _expected_output) = content.split_once(DELIMITER).unwrap();
		let actual_output = match std::panic::catch_unwind(|| f(input)) {
			Ok(s) => s,
			Err(e) => {
				println!("=== TEST PANICKED: {}", path.display());
				format!("{}\n", e.downcast::<String>().unwrap())
			}
		};
		let actual_content = format!("{input}{DELIMITER}{actual_output}");
		assert!(actual_content.ends_with('\n'));

		expect_test::expect_file![&path].assert_eq(&actual_content);
	}
}
