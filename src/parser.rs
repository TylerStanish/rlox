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
        unimplemented!();
    }

    fn primary(&mut self) -> Box<dyn Expression> {
        unimplemented!();
    }
}
