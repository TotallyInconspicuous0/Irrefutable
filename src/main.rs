use std::{fs::OpenOptions, io::Read};

use clap::Parser;
use lexer::{Lexer, IoBytesCharReader};

mod lexer;

#[derive(Parser, Debug)]
#[clap(version)]
pub struct Args {
    source_files: Vec<String>
}

fn main() {
    let args = Args::parse();

    if args.source_files.len() == 0 {
        eprintln!("No source files to compile");
        std::process::exit(1);
    }

    for path in &args.source_files {
        let file = match OpenOptions::new().read(true).open(path) {
            Ok(file) => file,
            Err(err) => {
                eprintln!("Could not open file {} for reading: {}", path, err);
                std::process::exit(1);
            }
        };

        let iterator = IoBytesCharReader::new(file.bytes());
        let mut lexer = Lexer::new(iterator);

        while let Some(token) = lexer.peek(0) {
            println!("{:?}", token);

            lexer.step();
        }
    }
}
