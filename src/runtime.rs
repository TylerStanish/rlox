use std::fs;
use std::io;
use std::io::BufRead;
use std::io::Write;

use crate::parser::Parser;
use crate::scanner::Scanner;

pub fn execute(code: &str) {
    let scanner = Scanner::new(code);
    let token_stream = scanner.scan();
    match token_stream {
        Ok(_) => (),
        Err(e) => {
            println!("{} at line {}", e.error, e.line_number);
            return;
        }
    };
    let mut parser = Parser::new(token_stream.unwrap());
    let expr = match parser.parse() {
        Ok(expr) => {
            println!("{}", expr);
            expr
        }
        Err(err) => match err.token {
            Some(token) => {
                println!("{} at line {}", err.message, token.line_number);
                return;
            }
            None => {
                println!("{}", err.message);
                return;
            }
        },
    };
    let res = expr.eval();
    println!("{:?}", res);
}

pub fn repl() {
    loop {
        print!("> ");
        io::stdout().flush().unwrap();
        let mut line = String::new();
        io::stdin().lock().read_line(&mut line).unwrap();
        execute(&line);
    }
}

pub fn interpreter(srcfile: &str) {
    let contents = match fs::read_to_string(srcfile) {
        Ok(contents) => contents,
        Err(e) => {
            println!("{}", e);
            return;
        }
    };
    execute(&contents);
}
