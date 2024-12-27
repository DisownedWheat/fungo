use chumsky::prelude::Parser;
use chumsky::Span;
use colored::*;
use fungo::lexer::lex;
use fungo::parser::{error_report, parser};
use std::ops::Range;
use type_checker::TypeChecker;
mod type_checker;
use serde_json::to_string_pretty;

fn main() {
    env_logger::builder()
        .filter_level(log::LevelFilter::Info)
        .init();
    let (tokens, _) = lex("./test_file").unwrap();
    for tok in &tokens {
        log::info!("{:?}", tok.0);
    }
    let ast = match parser().parse(tokens) {
        Ok(ast) => {
            let pretty = to_string_pretty(&ast).unwrap();
            println!("{}", pretty.cyan());
            ast
        }
        Err(e) => {
            e.iter().for_each(|e| {
                error_report(e);
            });
            panic!();
        }
    };

    let type_checker = TypeChecker::new();
    let modules = type_checker.get_modules(&ast);

    log::info!("{:?}", modules);
}
