use crate::tokens::Token;
use crate::tokens::TokenType;
use std::fmt;
use std::fmt::Formatter;
use std::iter::Peekable;
use std::vec::IntoIter;

pub trait Expression: fmt::Display {
    fn eval(&self);
}

struct BinaryExpression {
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

struct UnaryExpression {
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

struct LiteralExpression {
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

struct GroupingExpression {
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

pub struct ParsingError {
    pub token: Option<Token>,
    pub message: String,
}

impl ParsingError {
    pub fn new(token: Option<Token>, message: String) -> Self {
        ParsingError { token, message }
    }
}

pub struct Parser {
    tokens: Peekable<IntoIter<Token>>,
}

type ParsingResult = Result<Box<dyn Expression>, ParsingError>;

impl Parser {
    pub fn new(tokens: Vec<Token>) -> Self {
        Parser {
            tokens: tokens.into_iter().peekable(),
        }
    }

    pub fn parse(&mut self) -> ParsingResult {
        self.expression()
    }

    fn next_token_matches(&mut self, token_types: &[TokenType]) -> bool {
        match self.tokens.peek() {
            Some(token) => token_types.iter().any(|tt| *tt == token.token_type),
            None => false,
        }
    }

    fn expression(&mut self) -> ParsingResult {
        self.equality()
    }

    fn equality(&mut self) -> ParsingResult {
        self.parse_binary_operation(
            &[TokenType::BangEqual, TokenType::EqualEqual],
            &Parser::comparison,
        )
    }

    fn comparison(&mut self) -> ParsingResult {
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

    fn addition(&mut self) -> ParsingResult {
        self.parse_binary_operation(
            &[TokenType::Plus, TokenType::Minus],
            &Parser::multiplication,
        )
    }

    fn multiplication(&mut self) -> ParsingResult {
        self.parse_binary_operation(&[TokenType::Star, TokenType::Slash], &Parser::unary)
    }

    fn prioritize_higher_precedence_error(
        next_expr: ParsingResult,
        operator: &Token,
        message: String,
    ) -> ParsingResult {
        match next_expr {
            Ok(expr) => Ok(expr),
            Err(err) => match &err.token {
                Some(_) => return Err(err),
                None => return Err(ParsingError::new(Some(operator.clone()), message)),
            },
        }
    }

    fn parse_binary_operation(
        &mut self,
        operators: &[TokenType],
        next_expression: &dyn Fn(&mut Parser) -> ParsingResult,
    ) -> ParsingResult {
        let mut expression = next_expression(self)?;
        while self.next_token_matches(operators) {
            let operator = self.tokens.next().unwrap();
            // Previously (3e3d88) we overwrite the expression from the higher priority:
            /*
            let right_comparison = next_expression(self).or(Err(ParsingError::new(
                Some(operator.clone()),
                format!("Expected expression after '{}'", operator),
            )))?;
            */
            // We don't want to do that, but rather respect the error the higher-up
            // gave us and send that back.
            // EXCEPT when the error has a None token, then we want to provide a more
            // specific error.
            // As of (bdf8be) you do this:
            /*
            let right_comparison = match next_expression(self) {
                Ok(expr) => expr,
                Err(err) => match &err.token {
                    Some(_) => return Err(err),
                    None => {
                        return Err(ParsingError::new(
                            Some(operator.clone()),
                            format!("Expected expression after '{}'", operator),
                        ))
                    }
                },
            };
            */
            let right_comparison = Parser::prioritize_higher_precedence_error(
                next_expression(self),
                &operator,
                format!("Expected expression after '{}'", operator),
            )?;
            expression = Box::new(BinaryExpression::new(
                expression,
                operator.clone(),
                right_comparison,
            ));
        }
        Ok(expression)
    }

    fn unary(&mut self) -> ParsingResult {
        if self.next_token_matches(&[TokenType::Bang, TokenType::Minus]) {
            let operator = self.tokens.next().unwrap();
            let operand = Parser::prioritize_higher_precedence_error(
                self.unary(),
                &operator,
                format!("Expected expression after unary operator '{}'", operator),
            )?;
            return Ok(Box::new(UnaryExpression::new(operator, operand)));
        }
        self.primary()
    }

    fn primary(&mut self) -> ParsingResult {
        match self.tokens.next() {
            Some(token) => match token.token_type {
                TokenType::False | TokenType::True | TokenType::Eof | TokenType::Nil => {
                    Ok(Box::new(LiteralExpression::new(token)))
                }
                TokenType::Number(_) | TokenType::StringLiteral(_) => {
                    Ok(Box::new(LiteralExpression::new(token)))
                }
                TokenType::LeftParen => {
                    let expression = self.expression()?;
                    if self.next_token_matches(&[TokenType::RightParen]) {
                        self.tokens.next().unwrap();
                        Ok(Box::new(GroupingExpression::new(expression)))
                    } else {
                        self.synchronize();
                        Err(ParsingError::new(
                            Some(token),
                            "Expected closing ')' after '('".to_string(),
                        ))
                    }
                }
                other => {
                    // unexpected token
                    self.synchronize();
                    Err(ParsingError::new(
                        None,
                        format!("Unexpected token {}", other),
                    ))
                }
            },
            // empty
            None => Err(ParsingError::new(None, "Expected expression".to_string())),
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
