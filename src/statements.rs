use crate::expressions::{Expression, LoxObject};


pub enum Statement {
    StatementExpression(Expression),
    StatementPrint(Expression),
    StatementIf(Expression, Box<Statement>)
}

impl Statement {
    pub fn eval(&self) {
        match self {
            Statement::StatementExpression(expr) => {
                expr.eval();
            },
            Statement::StatementPrint(expr) => println!("{:?}", expr.eval()),
            Statement::StatementIf(condition, body) => {
                match condition.eval() {
                    LoxObject::LoxBoolean(b) => if b { body.eval(); },
                    other => panic!("Expected boolean condition in if statement, found {:?}", other),
                }
            }
        };
    }
}
