use crate::expressions::{Expression, LoxObject, Environment};
use crate::tokens::Token;

#[derive(PartialEq, Debug)]
pub enum Statement {
    StatementExpression(Expression),
    StatementPrint(Expression),
    StatementIf(Expression, Box<Statement>),
    StatementDeclaration(String, Expression),
    StatementBlock(Vec<Statement>),
    // TODO make these Strings Tokens?
}

impl Statement {
    pub fn eval(&self, scope: &mut Environment) {
        match self {
            Statement::StatementExpression(expr) => {
                expr.eval(scope);
            }
            Statement::StatementPrint(expr) => println!("{:?}", expr.eval(scope)),
            Statement::StatementIf(condition, body) => match condition.eval(scope) {
                LoxObject::LoxBoolean(b) => {
                    if b {
                        body.eval(scope);
                    }
                }
                other => panic!(
                    "Expected boolean condition in if statement, found {:?}",
                    other
                ),
            }
            Statement::StatementDeclaration(ident, val) => {
                let evaluated_expr = val.eval(scope);
                scope.declare(ident, &evaluated_expr).unwrap();
            }
            Statement::StatementBlock(statements) => {
                let mut new_scope = Environment::new();
                new_scope.next = Some(scope.clone().into());
                for statement in statements {
                    statement.eval(&mut new_scope);
                }
            }
        };
    }
}
