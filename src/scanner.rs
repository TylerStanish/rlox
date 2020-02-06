use std::str::{Chars};

use crate::tokens::{Token, TokenType};


#[derive(Debug, PartialEq)]
pub struct ScanningError {
    error: String,
    line_number: usize,
}
impl ScanningError {
    fn new(msg: String, line_number: usize) -> Self {
        ScanningError {
            error: msg,
            line_number,
        }
    }
}

pub struct Scanner<'a> {
    // might wanna make this IntoIter<char> instead so we own it and don't have to deal with lifetimes
    source: Box<Chars<'a>>,
    current_line: usize,

    tokens: Vec<Token>,
}

impl<'a> Scanner<'a> {
    pub fn new(code: &'a str) -> Self {
        let source = Box::new(code.chars());
        Scanner {
            source: source,
            current_line: 1,
            tokens: Vec::new(),
        }
    }

    fn parse_string_literal(&mut self) -> Result<Token, ScanningError> {
        let mut string = Vec::new();
        loop {
            let chr = match self.source.next() {
                Some(chr) => chr,
                None => return Err(ScanningError::new("Unterminated string literal".to_string(), self.current_line)),
            };
            match chr {
                '"' => break,
                // TODO handle double-quote escaping too
                chr => string.push(chr),
            };
        }
        Ok(Token::new(TokenType::StringLiteral(string.iter().collect()), self.current_line))
    }

    /// An Ok(Some(tok)) means we parsed ok and have a token.
    /// An Ok(None) means that we parsed ok but we have nothing to return, e.g. \n or \t.
    /// An Err(ScanningError) means there was a scanning error
    fn scan_token(&mut self) -> Result<Option<Token>, ScanningError> {
        let chr = match self.source.next() {
            Some(chr) => chr,
            None => return Ok(Some(Token::new(TokenType::Eof, self.current_line))),
        };
        match chr {
            '(' => Ok(Some(Token::new(TokenType::LeftParen, self.current_line))),
            ')' => Ok(Some(Token::new(TokenType::RightParen, self.current_line))),
            '{' => Ok(Some(Token::new(TokenType::LeftBrace, self.current_line))),
            '}' => Ok(Some(Token::new(TokenType::RightBrace, self.current_line))),
            '!' => Ok(Some(Token::new(TokenType::Bang, self.current_line))),
            '"' => match self.parse_string_literal() {
                Ok(tok) => Ok(Some(tok)),
                Err(e) => Err(e),
            },
            // parse tabs, whitespaces, newlines, etc
            '\n' => {
                self.current_line += 1;
                Ok(None)
            },
            '\r' | '\t' | ' ' => Ok(None),
            // must be a variable, keyword, etc
            // TODO change this? Return Err for unexpected token?
            other_chr => {
                Ok(None)
            },
        }
    }

    pub fn scan(mut self) -> Result<Vec<Token>, ScanningError> {
        loop {
            let res_opt_tok = self.scan_token();
            match res_opt_tok {
                Ok(opt_tok) => match opt_tok {
                    Some(tok) => match tok.token_type {
                        TokenType::Eof => return Ok(self.tokens),
                        _ => self.tokens.push(tok),
                    },
                    _ => (),
                },
                Err(e) => return Err(e),
            };
        };
    }
}



mod tests {
    use super::*;
    #[test]
    fn test_string_literal_parsing() {
        let input = r#"
            "hi"
        "#.trim();
        let actual_token_stream = Scanner::new(input).scan().unwrap();
        let expected_token_stream = vec![
            Token::new(TokenType::StringLiteral("hi".to_string()), 1),
        ];
        assert_eq!(expected_token_stream, actual_token_stream);
    }
    #[test]
    fn test_parentheses() {
        let input = r#"
            (())()()((()()))
        "#.trim();
        let actual_token_stream = Scanner::new(input).scan().unwrap();
        let expected_token_stream = vec![
            Token::new(TokenType::LeftParen, 1),
            Token::new(TokenType::LeftParen, 1),
            Token::new(TokenType::RightParen, 1),
            Token::new(TokenType::RightParen, 1),
            Token::new(TokenType::LeftParen, 1),
            Token::new(TokenType::RightParen, 1),
            Token::new(TokenType::LeftParen, 1),
            Token::new(TokenType::RightParen, 1),
            Token::new(TokenType::LeftParen, 1),
            Token::new(TokenType::LeftParen, 1),
            Token::new(TokenType::LeftParen, 1),
            Token::new(TokenType::RightParen, 1),
            Token::new(TokenType::LeftParen, 1),
            Token::new(TokenType::RightParen, 1),
            Token::new(TokenType::RightParen, 1),
            Token::new(TokenType::RightParen, 1),
        ];
        assert_eq!(expected_token_stream, actual_token_stream);
    }
    #[test]
    fn test_with_multiple_lines() {
        let input = r#"
            "hi"
            "there"
            {"how are you?"}
        "#.trim();
        let actual_token_stream = Scanner::new(input).scan().unwrap();
        let expected_token_stream = vec![
            Token::new(TokenType::StringLiteral("hi".to_string()), 1),
            Token::new(TokenType::StringLiteral("there".to_string()), 2),
            Token::new(TokenType::LeftBrace, 3),
            Token::new(TokenType::StringLiteral("how are you?".to_string()), 3),
            Token::new(TokenType::RightBrace, 3),
        ];
        assert_eq!(expected_token_stream, actual_token_stream);
    }

    #[test]
    fn test_basic_assignment() {
        let input = r#"
            var a = 1 + 2;
            var b = a * 5;
            var c = b - a;
        "#.trim();
        let actual_token_stream = Scanner::new(input).scan().unwrap();
        let expected_token_stream = vec![
            Token::new(TokenType::Var, 1),
            Token::new(TokenType::Identifier("a".to_string()), 1),
            Token::new(TokenType::Equal, 1),
            Token::new(TokenType::Number(1.0), 1),
            Token::new(TokenType::Plus, 1),
            Token::new(TokenType::Number(2.0), 1),
            Token::new(TokenType::Semicolon, 1),

            Token::new(TokenType::Var, 2),
            Token::new(TokenType::Identifier("b".to_string()), 2),
            Token::new(TokenType::Equal, 2),
            Token::new(TokenType::Identifier("a".to_string()), 2),
            Token::new(TokenType::Star, 2),
            Token::new(TokenType::Number(5.0), 2),
            Token::new(TokenType::Semicolon, 2),

            Token::new(TokenType::Var, 3),
            Token::new(TokenType::Identifier("c".to_string()), 3),
            Token::new(TokenType::Equal, 3),
            Token::new(TokenType::Identifier("a".to_string()), 3),
            Token::new(TokenType::Minus, 3),
            Token::new(TokenType::Identifier("b".to_string()), 3),
            Token::new(TokenType::Semicolon, 3),
        ];
        assert_eq!(expected_token_stream, actual_token_stream);
    }

    #[test]
    fn test_fails_with_scanning_error() {
        let input = r#"
            var bla = 42;
            "foobar
        "#.trim();
        let actual_token_stream = Scanner::new(input).scan();
        let err = actual_token_stream.expect_err("Should have told us there was an error");
        assert_eq!(err, ScanningError::new("Unterminated string literal".to_string(), 2));
    }
}
