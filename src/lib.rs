pub mod lexer;

fn error(loc: lexer::Loc, msg: String) {
	panic!("{:?}: error: {}", loc, msg);
}

#[cfg(test)]
mod testing;
