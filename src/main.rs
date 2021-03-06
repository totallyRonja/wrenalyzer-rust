pub mod data;
pub mod parser;

use data::Span;
use parser::module;
use std::fs;

fn main() {
	let contents = fs::read_to_string("test.wren").expect("couldn't find file");
	//let contents = "import \"test/one\" for Test, Dict, Blob";
	let input_span = Span::new(&contents);
	let out = module(input_span);

	match out {
		Err(err) => println!("{:#?}", err),
		Ok(ok) => println!("{:#?}", ok.1),
	};
}
