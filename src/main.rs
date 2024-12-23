use chumsky::prelude::Parser;
use colored::*;
use fungo::lexer::lex;
use fungo::parser::{error_report, parser};
use serde_json::to_string_pretty;

fn main() {
    env_logger::builder()
        .filter_level(log::LevelFilter::Info)
        .init();
    let tokens = lex("./test_file").unwrap();
    for tok in &tokens {
        log::info!("{:?}", tok.0);
    }
    match parser().parse(tokens) {
        Ok(ast) => {
            let pretty = to_string_pretty(&ast).unwrap();
            println!("{}", pretty.cyan());
        }
        Err(e) => {
            e.iter().for_each(|e| {
                error_report(e);
            });
        }
    }
}
