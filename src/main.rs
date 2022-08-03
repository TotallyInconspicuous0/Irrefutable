use std::{fs::OpenOptions, io::Read, collections::HashMap};

use analysis::{Function, AnalysisError};
use clap::Parser;
use lexer::{Lexer, IoBytesCharReader};
use ast::TopLevelDecl;
use parser::ParseResult;
use types::GlobalContext;

use crate::ast::AstToCodeString;

mod lexer;
mod parser;
mod analysis;
mod ast;
mod types;

#[derive(Parser, Debug)]
#[clap(version)]
pub struct Args {
    source_files: Vec<String>,

    #[clap(long)]
    emit_ast: bool
}

fn unwrap_analysis_error<T>(res: Result<T, AnalysisError>) -> T {
    match res {
        Err(err) => {
            println!("{} at {:?}", err.message(), err.span());
            std::process::exit(1);
        },
        Ok(val) => val
    }
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

        let mut globals = GlobalContext::new();

        // let mut top_level = Vec::new();
        let mut functions: HashMap<String, Function> = HashMap::new();
        while let Some(_) = lexer.peek(0) {
            let node = match TopLevelDecl::parse(&mut lexer) {
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
            };
            
            if args.emit_ast {
                // println!("{:#?}", top_level.last().unwrap());
                println!("{}", node.to_code_string());
            }

            match node {
                TopLevelDecl::FunctionDecl(decl) => {
                    if let Some(function) = functions.get_mut(&decl.name) {
                        unwrap_analysis_error(function.add_decl(decl));
                    } else {
                        let name = decl.name.clone();
                        let mut function = Function::new(&name);
                        function.add_decl(decl).unwrap();
                        functions.insert(name, function);
                    }
                }
                TopLevelDecl::FunctionDef(case) => {
                    if let Some(function) = functions.get_mut(&case.name) {
                        unwrap_analysis_error(function.add_case(case));
                    } else {
                        let name = case.name.clone();
                        let mut function = Function::new(&name);
                        unwrap_analysis_error(function.add_case(case));
                        functions.insert(name, function);
                    }
                }
            }
        }
        
        for (_, function) in functions {
            globals.add(function.name(), unwrap_analysis_error(function.typ()));
            unwrap_analysis_error(function.check(&globals));
        }
    }
}
