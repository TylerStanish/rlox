use crate::expressions::{Expression};


#[derive(Debug)]
pub trait Statement {
    //fn eval(&self) -> Option<LoxObject>;
    fn eval_statement(&self);
    //fn expression(&self) -> Box<dyn Expression>;
}

pub struct ExpressionStatement {
    pub expr: Box<dyn Expression>,
}

impl ExpressionStatement {
    pub fn new(expr: Box<dyn Expression>) -> Self {
        ExpressionStatement {
            expr
        }
    }
}

impl Statement for ExpressionStatement {
    fn eval_statement(&self) {
        self.expr.eval();
    }
}

pub struct PrintStatement {
    pub expr: Box<dyn Expression>,
}

impl Statement for PrintStatement {
    fn eval_statement(&self) {
        println!("{}", self.expr);
    }
}

pub struct IfStatement {
    pub condition: Box<dyn Expression>,
    pub body: Box<dyn Statement>,
}

impl Statement for IfStatement {
    fn eval_statement(&self) {
        let res: bool = self.condition.eval().into();
        if res {
            self.body.eval_statement();
        }
    }
}