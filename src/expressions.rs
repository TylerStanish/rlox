use std::collections::HashMap;

use crate::tokens::{Token, TokenType};

pub type Scope = HashMap<String, LoxObject>;

#[derive(Clone)]
pub struct Environment {
    pub values: Scope,
    pub next: Option<Box<Environment>>,
}

impl Environment {
    pub fn new() -> Self {
        Environment {
            values: HashMap::new(),
            next: None,
        }
    }

    pub fn get(&self, ident: &String) -> Option<LoxObject> {
        if self.values.contains_key(ident) {
            Some(self.values.get(ident).unwrap().clone())
        } else {
            match &self.next {
                Some(next) => next.get(ident),
                None => None
            }
        }
    }

    /// Returns Ok if a variable with this name exists already and was able to assign to it
    /// Returns Err if a variable with this name has not been declared
    pub fn assign(&mut self, ident: &String, val: &LoxObject) -> Result<(), ()> {
        if self.get(ident).is_none() {
            return Err(());
        }
        if self.values.contains_key(ident) {
            // update the value
            self.values.insert(ident.clone(), val.clone());
            return Ok(());
        }
        self.next.as_mut().unwrap().assign(ident, val)
    }

    /// Returns Ok if a variable with this name has not already been declared
    /// Returns Err if a variable with this name is already declared in this scope
    pub fn declare(&mut self, ident: &String, val: &LoxObject) -> Result<(), ()> {
        if self.values.contains_key(ident) {
            Err(())
        } else {
            self.values.insert(ident.clone(), val.clone());
            Ok(())
        }
    }
}

/// The builtin types of Lox
#[derive(Debug, Clone)]
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

#[derive(PartialEq, Debug)]
pub enum Expression {
    ExprBinary(Token, Box<Expression>, Box<Expression>),
    ExprUnary(Token, Box<Expression>),
    ExprLiteral(Token),
    ExprGrouping(Box<Expression>),
    ExprVariable(Token),
    // TODO make this a Token? Or LoxObject?
    ExprAssigment(Token, Box<Expression>),
}

impl Expression {
    pub fn eval(&self, scope: &mut Environment) -> LoxObject {
        match self {
            Expression::ExprBinary(op_tok, lhs, rhs) => match &op_tok.token_type {
                TokenType::Plus => {
                    let left_num: f64 = lhs.eval(scope).into();
                    let right_num: f64 = rhs.eval(scope).into();
                    LoxObject::LoxNumber(left_num + right_num)
                }
                TokenType::Minus => {
                    let left_num: f64 = lhs.eval(scope).into();
                    let right_num: f64 = rhs.eval(scope).into();
                    LoxObject::LoxNumber(left_num - right_num)
                }
                TokenType::Star => {
                    let left_num: f64 = lhs.eval(scope).into();
                    let right_num: f64 = rhs.eval(scope).into();
                    LoxObject::LoxNumber(left_num * right_num)
                }
                TokenType::Slash => {
                    let left_num: f64 = lhs.eval(scope).into();
                    let right_num: f64 = rhs.eval(scope).into();
                    LoxObject::LoxNumber(left_num / right_num)
                }
                other => panic!("{} is not a binary operator", other),
            },
            Expression::ExprUnary(op_tok, operand) => match &op_tok.token_type {
                TokenType::Bang => {
                    let res: bool = operand.eval(scope).into();
                    LoxObject::LoxBoolean(res)
                }
                TokenType::Minus => {
                    let res: f64 = operand.eval(scope).into();
                    LoxObject::LoxNumber(-res)
                }
                other => panic!("{} is not a unary operator", other),
            },
            Expression::ExprGrouping(expr) => expr.eval(scope),
            Expression::ExprLiteral(literal) => match &literal.token_type {
                TokenType::StringLiteral(s) => LoxObject::LoxString(s.clone()),
                TokenType::Number(n) => LoxObject::LoxNumber(*n),
                TokenType::True => LoxObject::LoxBoolean(true),
                TokenType::False => LoxObject::LoxBoolean(false),
                TokenType::Nil => LoxObject::LoxNil,
                other => panic!("Expected literal, found {}", other),
            },
            Expression::ExprVariable(var) => match &var.token_type {
                TokenType::Identifier(ident) => {
                    scope.get(ident).unwrap()
                }
                other => panic!("Expected identifier for variable, found {}", other)
            }
            Expression::ExprAssigment(tok, val) => match &tok.token_type {
                TokenType::Identifier(ident) => {
                    let evaluated_expr = val.eval(scope);
                    scope.assign(ident, &evaluated_expr).unwrap();
                    evaluated_expr
                }
                other => panic!("Expected identifier in assignment, found {}", other)
            }
        }
    }
}
