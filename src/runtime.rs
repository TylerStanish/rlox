use std::collections::HashMap;
use std::fs;
use std::io;
use std::io::BufRead;
use std::io::Write;

use crate::expressions::Environment;
use crate::parser::Parser;
use crate::scanner::Scanner;

pub fn execute(code: &str, global_environment: &mut Environment) {
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
    for stmt_res in parser.parse() {
        match stmt_res {
            Ok(stmt) => stmt.eval(global_environment),
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
        }
    }
}

pub fn repl() {
    let mut global_environment = Environment::new(HashMap::new());
    loop {
        print!("> ");
        io::stdout().flush().unwrap();
        let mut line = String::new();
        io::stdin().lock().read_line(&mut line).unwrap();
        execute(&line, &mut global_environment);
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
    let mut global_environment = Environment::new(HashMap::new());
    execute(&contents, &mut global_environment);
}
