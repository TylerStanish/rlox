use crate::expressions::{Expression};


pub trait Statement {
    //fn eval(&self) -> Option<LoxObject>;
    fn eval_statement(&self);
    //fn expression(&self) -> Box<dyn Expression>;
}

pub struct PrintStatement {
    pub expr: Box<dyn Expression>,
}

impl Statement for PrintStatement {
    fn eval_statement(&self) {
        println!("{}", self.expr);
    }
}