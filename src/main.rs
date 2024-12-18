use chumsky::prelude::Parser;
use colored::*;
use serde_json::to_string_pretty;
mod ast;
mod lexer;
mod parser;

fn main() {
    env_logger::builder()
        .filter_level(log::LevelFilter::Info)
        .init();
    let tokens = lexer::lex("./test_file").unwrap();
    for tok in &tokens {
        log::info!("{:?}", tok.kind);
    }
    match parser::parser().parse(tokens) {
        Ok(ast) => {
            let pretty = to_string_pretty(&ast).unwrap();
            println!("{}", pretty.cyan());
        }
        Err(e) => {
            e.iter().for_each(|e| {
                parser::error_report(e);
            });
        }
    }
}
