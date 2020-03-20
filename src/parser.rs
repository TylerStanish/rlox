use crate::tokens::Token;
use crate::tokens::TokenType;
use std::iter::Peekable;
use std::vec::IntoIter;

use crate::expressions::Expression;
use crate::statements::Statement;

#[derive(Debug, PartialEq)]
pub struct ParsingError {
    pub token: Option<Token>,
    pub message: String,
    pub priority: ParsingErrorPriority,
}

impl ParsingError {
    pub fn new(token: Option<Token>, message: String, priority: ParsingErrorPriority) -> Self {
        ParsingError { token, message, priority }
    }
}

#[derive(Debug, PartialOrd, PartialEq)]
pub enum ParsingErrorPriority {
    Primary, // = 0, lowest priority
    Unary,
    /*
    Multiplication,
    Addition,
    Comparison,
    Equality,
    */
    Binary,
    Statement, // = 7, highest priority
}

pub struct Parser {
    tokens: Peekable<IntoIter<Token>>,
}

type ParsingResult = Result<Statement, ParsingError>;
type ExpressionParsingResult = Result<Expression, ParsingError>;

impl Parser {
    pub fn new(tokens: Vec<Token>) -> Self {
        Parser {
            tokens: tokens.into_iter().peekable(),
        }
    }

    pub fn parse(&mut self) -> Vec<ParsingResult> {
        // https://www.reddit.com/r/rust/comments/94s9ys/why_cant_i_cast_a_dyn_a_into_a_dyn_b_when_b_a/
        let mut statements = Vec::new();
        while !self.is_at_end() {
            statements.push(self.declaration());
        }
        statements
    }

    fn is_at_end(&mut self) -> bool {
        match self.tokens.peek() {
            Some(token) => token.token_type == TokenType::Eof,
            None => true,
        }
    }

    fn next_token_matches(&mut self, token_types: &[TokenType]) -> bool {
        match self.tokens.peek() {
            Some(token) => token_types.iter().any(|tt| *tt == token.token_type),
            None => false,
        }
    }

    fn declaration(&mut self) -> ParsingResult {
        if self.next_token_matches(&[TokenType::Var]) {
            self.tokens.next().unwrap(); // consume the 'var'
            self.var_declaration()
        } else {
            self.statement()
        }
    }

    fn var_declaration(&mut self) -> ParsingResult {
        let ident = match &self.tokens.peek().unwrap().token_type {
            TokenType::Identifier(ident) => ident.clone(),
            _ => return Err(ParsingError::new(None, format!("Expected identifier after 'var' declaration").to_string(), ParsingErrorPriority::Statement))
        };
        self.tokens.next().unwrap();
        if !&self.next_token_matches(&[TokenType::Equal]) {
            return Err(ParsingError::new(None, format!("Expected '=' after 'var' declaration").to_string(), ParsingErrorPriority::Statement))
        }
        self.tokens.next().unwrap(); // consume '='
        let val = self.expression()?;
        if !self.next_token_matches(&[TokenType::Semicolon]) {
            return Err(ParsingError::new(None, format!("Expected ';' after 'var' declaration").to_string(), ParsingErrorPriority::Statement))
        }
        self.tokens.next().unwrap(); // consume ';'
        Ok(Statement::StatementDeclaration(ident.clone(), val))
    }

    fn statement(&mut self) -> ParsingResult {
        if self.next_token_matches(&[TokenType::Print]) {
            self.tokens.next().unwrap(); // consume 'print'
            self.print_statement()
        } else if self.next_token_matches(&[TokenType::If]) {
            self.tokens.next().unwrap(); // consume 'if'
            self.if_statement()
        } else {
            //let res = self.expression().map(|expr| Box::new(ExpressionStatement::new(expr)) as Box<dyn Statement>);
            let res = self.expression()?;
            if !self.next_token_matches(&[TokenType::Semicolon]) {
                return Err(ParsingError::new(None, format!("Expected ';' after expression statement").to_string(), ParsingErrorPriority::Statement));
            }
            self.tokens.next().unwrap();
            Ok(Statement::StatementExpression(res))
        }
    }

    fn print_statement(&mut self) -> ParsingResult {
        let expr = self.expression()?;
        if !self.next_token_matches(&[TokenType::Semicolon]) {
            return Err(ParsingError::new(None, "Expected ';' after print statement".to_string(), ParsingErrorPriority::Statement));
        }
        self.tokens.next().unwrap();
        Ok(Statement::StatementPrint(expr))
    }

    fn if_statement(&mut self) -> ParsingResult {
        if !self.next_token_matches(&[TokenType::LeftParen]) {
            return Err(ParsingError::new(None, "Expected '(' after 'if'".to_string(), ParsingErrorPriority::Statement));
        }
        self.tokens.next().unwrap(); // consume (
        let condition = self.expression()?;
        if !self.next_token_matches(&[TokenType::RightParen]) {
            return Err(ParsingError::new(None, "Expected ')' after if condition".to_string(), ParsingErrorPriority::Statement));
        }
        self.tokens.next().unwrap(); // consume )
        if !self.next_token_matches(&[TokenType::LeftBrace]) {
            return Err(ParsingError::new(None, "Expected '{' after ')' in if statement".to_string(), ParsingErrorPriority::Statement));
        }
        self.tokens.next().unwrap(); // consume {
        let body = self.statement()?;
        if !self.next_token_matches(&[TokenType::RightBrace]) {
            return Err(ParsingError::new(None, "Expected '}' after if statement body".to_string(), ParsingErrorPriority::Statement));
        }
        self.tokens.next().unwrap(); // consume }
        Ok(Statement::StatementIf(condition, body.into()))
    }

    fn expression(&mut self) -> ExpressionParsingResult {
        if self.next_token_matches(&[TokenType::Eof]) {
            Ok(Expression::ExprLiteral(self.tokens.next().unwrap()))
        } else {
            self.equality()
        }
    }

    fn equality(&mut self) -> ExpressionParsingResult {
        self.parse_binary_operation(
            &[TokenType::BangEqual, TokenType::EqualEqual],
            &Parser::comparison,
        )
    }

    fn comparison(&mut self) -> ExpressionParsingResult {
        self.parse_binary_operation(
            &[
                TokenType::LessEqual,
                TokenType::Less,
                TokenType::GreaterEqual,
                TokenType::Greater,
            ],
            &Parser::addition,
        )
    }

    fn addition(&mut self) -> ExpressionParsingResult {
        self.parse_binary_operation(
            &[TokenType::Plus, TokenType::Minus],
            &Parser::multiplication,
        )
    }

    fn multiplication(&mut self) -> ExpressionParsingResult {
        self.parse_binary_operation(&[TokenType::Star, TokenType::Slash], &Parser::unary)
    }

    fn prioritize_higher_precedence_error(
        next_expr: ExpressionParsingResult,
        operator: &Token,
        message: String,
        priority_here: ParsingErrorPriority,
    ) -> ExpressionParsingResult {
        match next_expr {
            Ok(expr) => Ok(expr),
            Err(err) => {
                if err.priority < priority_here {
                    Err(ParsingError::new(Some(operator.clone()), message, err.priority))
                } else {
                    Err(err)
                }
            },
        }
    }

    fn parse_binary_operation(
        &mut self,
        operators: &[TokenType],
        next_expression: &dyn Fn(&mut Parser) -> ExpressionParsingResult,
    ) -> ExpressionParsingResult {
        let mut expression = next_expression(self)?;
        while self.next_token_matches(operators) {
            let operator = self.tokens.next().unwrap();
            let right_comparison = Parser::prioritize_higher_precedence_error(
                next_expression(self),
                &operator,
                format!("Expected expression after '{}'", operator),
                ParsingErrorPriority::Binary,
            )?;
            expression = Expression::ExprBinary(operator.clone(), expression.into(), right_comparison.into())
        }
        Ok(expression)
    }

    fn unary(&mut self) -> ExpressionParsingResult {
        if self.next_token_matches(&[TokenType::Bang, TokenType::Minus]) {
            let operator = self.tokens.next().unwrap();
            let operand = Parser::prioritize_higher_precedence_error(
                self.unary(),
                &operator,
                format!("Expected expression after unary operator '{}'", operator),
                ParsingErrorPriority::Unary,
            )?;
            return Ok(Expression::ExprUnary(operator, operand.into()))
        }
        self.primary()
    }

    fn primary(&mut self) -> ExpressionParsingResult {
        match &self.tokens.next() {
            Some(token) => match &token.token_type {
                TokenType::False | TokenType::True | TokenType::Nil => {
                    Ok(Expression::ExprLiteral(token.clone()))
                }
                TokenType::Number(_) | TokenType::StringLiteral(_) => {
                    Ok(Expression::ExprLiteral(token.clone()))
                }
                TokenType::LeftParen => {
                    let expression = self.expression()?;
                    if self.next_token_matches(&[TokenType::RightParen]) {
                        self.tokens.next().unwrap();
                        Ok(Expression::ExprGrouping(expression.into()))
                    } else {
                        self.synchronize();
                        Err(ParsingError::new(
                            Some(token.clone()),
                            "Expected closing ')' after '('".to_string(),
                            ParsingErrorPriority::Primary,
                        ))
                    }
                }
                TokenType::Eof => {
                    Err(ParsingError::new(
                        None,
                        "Unexpected EOF".to_string(),
                        ParsingErrorPriority::Primary,
                    ))
                }
                other => {
                    // unexpected token
                    self.synchronize();
                    Err(ParsingError::new(
                        Some(token.clone()),
                        format!("Unexpected token {}", other),
                        ParsingErrorPriority::Primary,
                    ))
                }
            },
            // empty
            //None => Err(ParsingError::new(None, "Expected expression".to_string(), ParsingErrorPriority::Primary)),
            //None => Ok(Box::new(LiteralExpression::new(Token::new(TokenType::Nil, 0)))),
            None => panic!("Lexing error, lexer should put TokenType::Eof at the end of the lexing stage"),
        }
    }

    /// Calls next() until we reach a semicolon or statement boundary.
    /// Then continue to parse
    fn synchronize(&mut self) {
        loop {
            match self.tokens.next() {
                Some(token) => match token.token_type {
                    TokenType::Semicolon
                    | TokenType::Fun
                    | TokenType::Var
                    | TokenType::For
                    | TokenType::If
                    | TokenType::While
                    | TokenType::Print
                    | TokenType::Return => return,
                    _ => (),
                },
                None => return,
            }
        }
    }
}

mod tests {
    #[cfg(test)]
    use super::*;
    use crate::scanner::Scanner;
    #[cfg(test)]
    use pretty_assertions::assert_eq;

    #[test]
    fn test_string_literal_parsing() {
        let input = r#"
            "hi";
        "#
        .trim();
        let token_stream = Scanner::new(input).scan().unwrap();
        let actual_ast = Parser::new(token_stream).parse();
        let expected_ast = vec![
            Ok(Statement::StatementExpression(Expression::ExprLiteral(Token::new(TokenType::StringLiteral("hi".to_string()), 1))))
        ];
        assert_eq!(expected_ast, actual_ast);
    }
}
