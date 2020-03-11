use std::fmt;
use std::fmt::Formatter;

use crate::tokens::{Token, TokenType};

/// The builtin types of Lox
#[derive(Debug)]
pub enum LoxObject {
    LoxNumber(f64),
    LoxString(String),
    LoxBoolean(bool),
    LoxNil,
    LoxFunction,
}

impl From<LoxObject> for f64 {
    fn from(lox_obj: LoxObject) -> f64 {
        match lox_obj {
            LoxObject::LoxNumber(num) => num,
            _ => 0.0, // TODO runtime error, cannot convert non-number to float? Or just do binary magic?
        }
    }
}

impl From<LoxObject> for bool {
    fn from(lox_obj: LoxObject) -> bool {
        match lox_obj {
            LoxObject::LoxNumber(num) => num != 0.0, // coerce non-zero numbers to true, zero to false
            LoxObject::LoxString(s) => !s.is_empty(), // coerce non-empty strings to true, empty strings to false
            LoxObject::LoxBoolean(b) => b,
            LoxObject::LoxNil => false,
            LoxObject::LoxFunction => true, // coerce all functions to true, i guess lol, or TODO raise runtime error
        }
    }
}

pub trait Expression: fmt::Display {
    fn eval(&self) -> LoxObject;
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
    fn eval(&self) -> LoxObject {
        match self.operator.token_type {
            TokenType::Plus => {
                let left_num: f64 = self.left.eval().into();
                let right_num: f64 = self.right.eval().into();
                LoxObject::LoxNumber(left_num + right_num)
            },
            TokenType::Minus => {
                let left_num: f64 = self.left.eval().into();
                let right_num: f64 = self.right.eval().into();
                LoxObject::LoxNumber(left_num - right_num)
            },
            TokenType::Star => {
                let left_num: f64 = self.left.eval().into();
                let right_num: f64 = self.right.eval().into();
                LoxObject::LoxNumber(left_num * right_num)
            },
            TokenType::Slash => {
                let left_num: f64 = self.left.eval().into();
                let right_num: f64 = self.right.eval().into();
                LoxObject::LoxNumber(left_num / right_num)
            },
            _ => panic!(format!("Expected one of ['+', '-', '*', '/'] for binary operation, got {}", self.operator.token_type)), // runtime error (not our fault! user's fault!)
        }
    }
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
    fn eval(&self) -> LoxObject {
        match self.operator.token_type {
            TokenType::Minus => {
                let operand: f64 = self.operand.eval().into();
                LoxObject::LoxNumber(-operand)
            },
            TokenType::Bang => {
                let operand: bool = self.operand.eval().into();
                LoxObject::LoxBoolean(!operand)
            },
            _ => panic!(format!("Expected one of ['-', '!'] for binary operation, got {}", self.operator.token_type)), // runtime error (not our fault! user's fault!)
        }
    }
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
    fn eval(&self) -> LoxObject {
        match &self.literal.token_type {
            TokenType::Number(num) => LoxObject::LoxNumber(*num),
            TokenType::StringLiteral(s) => LoxObject::LoxString(s.clone()),
            TokenType::Eof => LoxObject::LoxNil,
            _ => panic!(format!("Expected a literal token type, got {}", self.literal.token_type)), // runtime error (not our fault! user's fault!)
        }
    }
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
    fn eval(&self) -> LoxObject {
        self.expression.eval()
    }
}
