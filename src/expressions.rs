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

pub enum Expression {
    ExprBinary(Token, Box<Expression>, Box<Expression>),
    ExprUnary(Token, Box<Expression>),
    ExprLiteral(Token),
    ExprGrouping(Box<Expression>),
}

impl Expression {
    pub fn eval(&self) -> LoxObject {
        match self {
            Expression::ExprBinary(op_tok, lhs, rhs) => match &op_tok.token_type {
                TokenType::Plus => {
                    let left_num: f64 = lhs.eval().into();
                    let right_num: f64 = rhs.eval().into();
                    LoxObject::LoxNumber(left_num + right_num)
                },
                TokenType::Minus => {
                    let left_num: f64 = lhs.eval().into();
                    let right_num: f64 = rhs.eval().into();
                    LoxObject::LoxNumber(left_num - right_num)
                },
                TokenType::Star => {
                    let left_num: f64 = lhs.eval().into();
                    let right_num: f64 = rhs.eval().into();
                    LoxObject::LoxNumber(left_num * right_num)
                },
                TokenType::Slash => {
                    let left_num: f64 = lhs.eval().into();
                    let right_num: f64 = rhs.eval().into();
                    LoxObject::LoxNumber(left_num / right_num)
                },
                other => panic!("{} is not a binary operator", other)
            }
            Expression::ExprUnary(op_tok, operand) => match &op_tok.token_type {
                TokenType::Bang => {
                    let res: bool = operand.eval().into();
                    LoxObject::LoxBoolean(res)
                },
                TokenType::Minus => {
                    let res: f64 = operand.eval().into();
                    LoxObject::LoxNumber(-res)
                },
                other => panic!("{} is not a unary operator", other)
            },
            Expression::ExprGrouping(expr) => expr.eval(),
            Expression::ExprLiteral(literal) => match &literal.token_type {
                TokenType::StringLiteral(s) => LoxObject::LoxString(s.clone()),
                TokenType::Number(n) => LoxObject::LoxNumber(*n),
                TokenType::True => LoxObject::LoxBoolean(true),
                TokenType::False => LoxObject::LoxBoolean(false),
                TokenType::Nil => LoxObject::LoxNil,
                other => panic!("Expected literal, found {}", other),
            }
        }
    }
}
