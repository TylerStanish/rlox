use std::str::{Chars, FromStr};

use crate::tokens::{Token, TokenType};


#[derive(Debug)]
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
    start: usize, // first char in the current lexeme
    // NOTE be careful about unicode with these start and current offsets?!?!?
    current: usize, // where we are in scannnig the current lexeme
    current_line: usize,

    tokens: Vec<Token>,
}

impl<'a> Scanner<'a> {
    pub fn new(code: &'a str) -> Self {
        let source = Box::new(code.chars());
        Scanner {
            source: source,
            start: 0,
            current: 0,
            current_line: 1,
            tokens: Vec::new(),
        }
    }

    fn parse_string_literal(&mut self) -> Result<Token, ScanningError> {
        let mut string = Vec::new();
        loop {
            let chr = match self.source.next() {
                Some(chr) => chr,
                None => return Err(ScanningError::new("Unterinated string literal".to_string(), self.current_line)),
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
        // we can string-index if we use ascii, otherwise strings can have variable
        // chars and therefore we need an iterator to get the nth char in the string in Rust
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
            '\r' => Ok(None),
            '\t' => Ok(None),
            ' ' => Ok(None),
            // must be a variable, keyword, etc
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
    fn test_basic_lexing_with_multiple_lines() {
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
}
