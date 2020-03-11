use std::env;

mod expressions;
mod parser;
mod runtime;
mod scanner;
mod tokens;

fn main() {
    let args: Vec<String> = env::args().collect();
    match args.len() {
        1 => runtime::repl(),
        2 => runtime::interpreter(args.get(1).unwrap()),
        _ => println!("Usage: lox [filename]"),
    }
}
