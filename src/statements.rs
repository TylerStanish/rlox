use std::fmt;
use std::fmt::Formatter;

use crate::expressions::{Expression};


pub enum Statement {
    StatementExpression(Expression),
    StatementPrint(Expression),
    StatementIf(Expression, Box<Statement>)
}

impl Statement {
    fn eval(&self) {
        match self {
            Statement::StatementExpression(expr) => expr.eval(),
            Statement::PrintStatement(expr) => println!("{}", expr.eval()),
            Statement::IfStatement(condition, body) => {
                if condition.eval() {
                    body.eval();
                }
            }
        }
    }
}
