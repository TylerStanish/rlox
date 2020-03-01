use crate::tokens::Token;
use crate::tokens::TokenType;
use std::iter::Peekable;
use std::vec::IntoIter;

trait Expression {
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

impl Expression for GroupingExpression {
    fn eval(&self) {}
}

struct ParsingError {}

struct Parser {
    tokens: Peekable<IntoIter<Token>>,
}

impl Parser {
    pub fn new(tokens: Vec<Token>) -> Self {
        Parser {
            tokens: tokens.into_iter().peekable(),
        }
    }

    pub fn parse(&mut self) -> Box<dyn Expression> {
        self.expression()
    }

    fn next_token_matches(&mut self, token_types: &[TokenType]) -> bool {
        match self.tokens.peek() {
            Some(token) => token_types.iter().any(|tt| *tt == token.token_type),
            None => false,
        }
    }

    fn expression(&mut self) -> Box<dyn Expression> {
        self.equality()
    }

    fn equality(&mut self) -> Box<dyn Expression> {
        self.parse_binary_operation(
            &[TokenType::BangEqual, TokenType::EqualEqual],
            &Parser::comparison,
        )
    }

    fn comparison(&mut self) -> Box<dyn Expression> {
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

    fn addition(&mut self) -> Box<dyn Expression> {
        self.parse_binary_operation(
            &[TokenType::Plus, TokenType::Minus],
            &Parser::multiplication,
        )
    }

    fn multiplication(&mut self) -> Box<dyn Expression> {
        self.parse_binary_operation(&[TokenType::Star, TokenType::Slash], &Parser::unary)
    }

    fn parse_binary_operation(
        &mut self,
        operators: &[TokenType],
        next_expression: &dyn Fn(&mut Parser) -> Box<dyn Expression>,
    ) -> Box<dyn Expression> {
        let mut expression = next_expression(self);
        while self.next_token_matches(operators) {
            let operator = self.tokens.next().unwrap();
            let right_comparison = next_expression(self);
            expression = Box::new(BinaryExpression::new(
                expression,
                operator,
                right_comparison,
            ));
        }
        expression
    }

    fn unary(&mut self) -> Box<dyn Expression> {
        if self.next_token_matches(&[TokenType::Bang, TokenType::Minus]) {
            let operator = self.tokens.next().unwrap();
            let operand = self.unary();
            return Box::new(UnaryExpression::new(operator, operand));
        }
        self.primary()
    }

    fn primary(&mut self) -> Box<dyn Expression> {
        match self.tokens.next() {
            Some(token) => match token.token_type {
                TokenType::False | TokenType::True | TokenType::Eof | TokenType::Nil => {
                    Box::new(LiteralExpression::new(token))
                }
                TokenType::Number(_) | TokenType::StringLiteral(_) => {
                    Box::new(LiteralExpression::new(token))
                }
                TokenType::LeftParen => {
                    let expression = self.expression();
                    if self.next_token_matches(&[TokenType::RightParen]) {
                        self.tokens.next().unwrap();
                        return Box::new(GroupingExpression::new(expression));
                    } else {
                        self.synchronize();
                        self.expression()
                    }
                }
                _ => {
                    self.synchronize();
                    self.expression()
                }
            },
            // empty
            None => Box::new(LiteralExpression::new(Token::new(TokenType::Eof, 1))),
        }
    }

    /// Calls next() until we reach a semicolon. Then continue to parse
    fn synchronize(&mut self) {}
}
