use std::{fs::OpenOptions, io::Read};

use clap::Parser;
use lexer::{Lexer, IoBytesCharReader};
use parser::{TopLevelDecl, ParseResult};

mod lexer;
mod parser;

#[derive(Parser, Debug)]
#[clap(version)]
pub struct Args {
    source_files: Vec<String>,

    #[clap(long)]
    emit_ast: bool
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

        let mut top_level = Vec::new();
        while let Some(_) = lexer.peek(0) {
            top_level.push(match TopLevelDecl::parse(&mut lexer) {
                ParseResult::NoMatch => {
                    eprint!("Unexpected token '{:?}'", lexer.peek(0));
                    eprintln!(" at {:?}", lexer.peek_span(0));
                    std::process::exit(1);
                }
                ParseResult::Err(err) => {
                    match err.span() {
                        Some(span) => {
                            eprint!("Error at {:?}", span);
                        }
                        None => {
                            eprint!("Unexpected eof");
                        }
                    }
                    eprintln!(", {}", err.message());
                    std::process::exit(1);
                }
                ParseResult::Match(node) => node
            });
            
            if args.emit_ast {
                println!("{:#?}", top_level.last().unwrap());
            }
        }
    }
}
