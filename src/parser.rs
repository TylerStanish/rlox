use std::fmt::Formatter;
use crate::tokens::Token;
use crate::tokens::TokenType;
use std::fmt;
use std::iter::Peekable;
use std::vec::IntoIter;

trait Expression: fmt::Display {
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

struct ParsingError {}

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

    pub fn parse(&mut self) {
        match self.expression() {
            Ok(expr) => println!("{}", expr),
            Err(err) => println!("Failed to parse"),
        }
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

    fn parse_binary_operation(
        &mut self,
        operators: &[TokenType],
        next_expression: &dyn Fn(&mut Parser) -> ParsingResult,
    ) -> ParsingResult {
        let mut expression = next_expression(self)?;
        while self.next_token_matches(operators) {
            let operator = self.tokens.next().unwrap();
            let right_comparison = next_expression(self)?;
            expression = Box::new(BinaryExpression::new(
                expression,
                operator,
                right_comparison,
            ));
        }
        Ok(expression)
    }

    fn unary(&mut self) -> ParsingResult {
        if self.next_token_matches(&[TokenType::Bang, TokenType::Minus]) {
            let operator = self.tokens.next().unwrap();
            let operand = self.unary()?;
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
                        self.expression()
                    }
                }
                _ => {
                    // unexpected token
                    self.synchronize();
                    self.expression()
                }
            },
            // empty
            None => Err(ParsingError{}),
        }
    }

    /// Calls next() until we reach a semicolon. Then continue to parse
    fn synchronize(&mut self) {}
}
