use crate::expressions::{Expression, LoxObject};
use crate::tokens::Token;

#[derive(PartialEq, Debug)]
pub enum Statement {
    StatementExpression(Expression),
    StatementPrint(Expression),
    StatementIf(Expression, Box<Statement>),
    StatementDeclaration(String, Expression),
    StatementAssigment(Token, Expression),
}

impl Statement {
    pub fn eval(&self) {
        match self {
            Statement::StatementExpression(expr) => {
                expr.eval();
            }
            Statement::StatementPrint(expr) => println!("{:?}", expr.eval()),
            Statement::StatementIf(condition, body) => match condition.eval() {
                LoxObject::LoxBoolean(b) => {
                    if b {
                        body.eval();
                    }
                }
                other => panic!(
                    "Expected boolean condition in if statement, found {:?}",
                    other
                ),
            },
            Statement::StatementDeclaration(ident, val) => {}
            Statement::StatementAssigment(tok, val) => {}
        };
    }
}
