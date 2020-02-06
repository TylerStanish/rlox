use std::io::BufRead;
use std::io;
use std::io::Write;
use std::fs;

use crate::scanner::Scanner;

pub fn execute(code: &str) {
    let scanner = Scanner::new(code);
    println!("{:?}", scanner.scan());
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
