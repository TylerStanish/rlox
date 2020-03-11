use std::fmt;
use std::fmt::Formatter;

use crate::tokens::Token;

pub trait Expression: fmt::Display {
    fn eval(&self);
}

pub struct BinaryExpression {
    left: Box<dyn Expression>,
    operator: Token,
    right: Box<dyn Expression>,
}

impl BinaryExpression {
    pub fn new(left: Box<dyn Expression>, operator: Token, right: Box<dyn Expression>) -> Self {
        BinaryExpression {
            left,
            operator,
            right,
        }
    }
}

// https://doc.rust-lang.org/book/ch19-03-advanced-traits.html#using-supertraits-to-require-one-traits-functionality-within-another-trait
impl fmt::Display for BinaryExpression {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "({} {} {})", self.operator, self.left, self.right)
    }
}

impl Expression for BinaryExpression {
    fn eval(&self) {}
}

pub struct UnaryExpression {
    operator: Token,
    operand: Box<dyn Expression>,
}

impl UnaryExpression {
    pub fn new(operator: Token, operand: Box<dyn Expression>) -> Self {
        UnaryExpression { operator, operand }
    }
}

impl fmt::Display for UnaryExpression {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "({} {})", self.operator, self.operand)
    }
}

impl Expression for UnaryExpression {
    fn eval(&self) {}
}

pub struct LiteralExpression {
    literal: Token,
}

impl LiteralExpression {
    pub fn new(literal: Token) -> Self {
        LiteralExpression { literal }
    }
}

impl fmt::Display for LiteralExpression {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.literal)
    }
}

impl Expression for LiteralExpression {
    fn eval(&self) {}
}

pub struct GroupingExpression {
    expression: Box<dyn Expression>,
}

impl GroupingExpression {
    pub fn new(expression: Box<dyn Expression>) -> Self {
        GroupingExpression { expression }
    }
}

impl fmt::Display for GroupingExpression {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "({})", self.expression)
    }
}

impl Expression for GroupingExpression {
    fn eval(&self) {}
}
