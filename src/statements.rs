use std::fmt;
use std::fmt::Formatter;

use crate::expressions::{Expression};


pub trait Statement: fmt::Display {
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

impl fmt::Display for ExpressionStatement {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.expr)
    }
}


pub struct PrintStatement {
    pub expr: Box<dyn Expression>,
}

impl PrintStatement {
    pub fn new(expr: Box<dyn Expression>) -> Self {
        PrintStatement {
            expr
        }
    }
}

impl Statement for PrintStatement {
    fn eval_statement(&self) {
        println!("{:?}", self.expr.eval());
    }
}

impl fmt::Display for PrintStatement {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "print {};", self.expr)
    }
}

pub struct IfStatement {
    pub condition: Box<dyn Expression>,
    pub body: Box<dyn Statement>,
}

impl IfStatement {
    pub fn new(condition: Box<dyn Expression>, body: Box<dyn Statement>) -> Self {
        Self {
            condition,
            body,
        }
    }
}

impl Statement for IfStatement {
    fn eval_statement(&self) {
        let res: bool = self.condition.eval().into();
        if res {
            self.body.eval_statement();
        }
    }
}

impl fmt::Display for IfStatement {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "if ({}) {{ {} }}", self.condition, self.body)
    }
}
