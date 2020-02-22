use std::iter::Peekable;
use std::vec::IntoIter;

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

pub struct Scanner {
    source: Peekable<IntoIter<char>>,
    current_line: usize,

    tokens: Vec<Token>,
}

impl Scanner {
    pub fn new(code: &str) -> Self {
        let source = code.chars().collect::<Vec<_>>().into_iter().peekable();
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
                '\\' => match self.source.next() {
                    Some(escaped) => match escaped {
                        '\\' => string.push('\\'),
                        '"' => string.push('"'),
                        other => return Err(ScanningError::new(format!("Unrecognized escape sequence {}", other), self.current_line)),
                    },
                    None => return Err(ScanningError::new("Unterminated escape character '\'".to_string(), self.current_line)),
                }
                chr => string.push(chr),
            };
        }
        Ok(Token::new(TokenType::StringLiteral(string.iter().collect()), self.current_line))
    }

    /// This can be a single-line comment, or division
    /// operator.
    fn parse_slash(&mut self) -> Result<Option<Token>, ScanningError> {
        let chr = match self.source.peek() {
            Some(chr) => chr,
            None => return Err(ScanningError::new("Dangling '/'".to_string(), self.current_line)),
        };
        match chr {
            '/' => {
                // single-line comment; go until \n or EOF (i.e. when 'while let' no longer matches Some(chr))
                while let Some(chr) = self.source.next() {
                    if chr == '\n' {
                        break
                    }
                }
                Ok(None)
            },
            _ => Ok(Some(Token::new(TokenType::Slash, self.current_line)))
        }
    }

    fn parse_lookahead_equal(&mut self, curr_char: char, single: TokenType, single_with_equals: TokenType) -> Result<Option<Token>, ScanningError> {
        let chr = match self.source.peek() {
            Some(chr) => chr,
            None => return Err(ScanningError::new(format!("Dangling {}", curr_char).to_string(), self.current_line)),
        };
        match chr {
            '=' => Ok(Some(Token::new(single_with_equals, self.current_line))),
            _ => Ok(Some(Token::new(single, self.current_line))),
        }
    }

    fn parse_number(&mut self, curr_char: char) -> Result<Option<Token>, ScanningError> {
        let mut string = vec![curr_char];
        while let Some(chr) = self.source.peek() {
            if *chr != '.' && (*chr < '0' || *chr > '9') {
                break
            }
            string.push(*chr);
            self.source.next();
        }
        // parse string
        let s: String = string.into_iter().collect();
        let num: f64 = match s.parse() {
            Ok(num) => num,
            Err(_) => return Err(ScanningError::new("Badly formed number".to_string(), self.current_line)),
        };
        Ok(Some(Token::new(TokenType::Number(num), self.current_line)))
    }

    /// checks if a string is a keyword, or just an identifier
    fn identifier_or_keyword(&self, s: &str) -> Token {
        match s {
            "and"    => Token::new(TokenType::And, self.current_line),
            "class"  => Token::new(TokenType::Class, self.current_line),
            "else"   => Token::new(TokenType::Else, self.current_line),
            "false"  => Token::new(TokenType::False, self.current_line),
            "fun"    => Token::new(TokenType::Fun, self.current_line),
            "for"    => Token::new(TokenType::For, self.current_line),
            "if"     => Token::new(TokenType::If, self.current_line),
            "nil"    => Token::new(TokenType::Nil, self.current_line),
            "or"     => Token::new(TokenType::Or, self.current_line),
            "print"  => Token::new(TokenType::Print, self.current_line),
            "return" => Token::new(TokenType::Return, self.current_line),
            "super"  => Token::new(TokenType::Super, self.current_line),
            "this"   => Token::new(TokenType::This, self.current_line),
            "true"   => Token::new(TokenType::True, self.current_line),
            "var"    => Token::new(TokenType::Var, self.current_line),
            "while"  => Token::new(TokenType::While, self.current_line),
            identifier => Token::new(TokenType::Identifier(identifier.to_string()), self.current_line)
        }
    }

    fn parse_identifier_or_keyword(&mut self, curr_char: char) -> Result<Option<Token>, ScanningError> {
        let mut string = vec![curr_char];
        // while we equal _, a-z, A-Z, or 0-9 we want to add to the string
        while let Some(chr) = self.source.peek() {
            if *chr != '_' && (*chr < 'a' || *chr > 'z') && (*chr < 'A' || *chr > 'Z') && (*chr < '0' || *chr > '9') {
                break;
            }
            string.push(*chr);
            self.source.next();
        }
        return Ok(Some(self.identifier_or_keyword(string.into_iter().collect::<String>().as_ref())))
    }

    fn parse_other(&mut self, curr_char: char) -> Result<Option<Token>, ScanningError> {
        if curr_char >= '0' && curr_char <= '9' {
            return self.parse_number(curr_char);
        }
        if (curr_char >= 'A' && curr_char <= 'Z') || (curr_char >= 'a' && curr_char <= 'z') || (curr_char == '_') {
            return self.parse_identifier_or_keyword(curr_char);
        }
        Err(ScanningError::new(format!("Unexpected token {}", curr_char), self.current_line))
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
            ',' => Ok(Some(Token::new(TokenType::Comma, self.current_line))),
            // don't need to consider numbers like '.5' because we don't allow that
            '.' => Ok(Some(Token::new(TokenType::Dot, self.current_line))),
            '-' => Ok(Some(Token::new(TokenType::Minus, self.current_line))),
            '+' => Ok(Some(Token::new(TokenType::Plus, self.current_line))),
            ';' => Ok(Some(Token::new(TokenType::Semicolon, self.current_line))),
            '/' => self.parse_slash(),
            '*' => Ok(Some(Token::new(TokenType::Star, self.current_line))),
            '!' => self.parse_lookahead_equal('!', TokenType::Bang, TokenType::BangEqual),
            '=' => self.parse_lookahead_equal('=', TokenType::Equal, TokenType::EqualEqual),
            '<' => self.parse_lookahead_equal('<', TokenType::Less, TokenType::LessEqual),
            '>' => self.parse_lookahead_equal('>', TokenType::Greater, TokenType::GreaterEqual),
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
            // must be a variable, keyword, or number
            other => self.parse_other(other),
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
    #[cfg(test)]
    use super::*;
    #[cfg(test)]
    use pretty_assertions::{assert_eq};

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
            Token::new(TokenType::Identifier("b".to_string()), 3),
            Token::new(TokenType::Minus, 3),
            Token::new(TokenType::Identifier("a".to_string()), 3),
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

    #[test]
    fn test_basic_keyword() {
        let input = r#"
            class B {}
        "#.trim();
        let actual_token_stream = Scanner::new(input).scan().unwrap();
        let expected_token_stream = vec![
            Token::new(TokenType::Class, 1),
            Token::new(TokenType::Identifier("B".to_string()), 1),
            Token::new(TokenType::LeftBrace, 1),
            Token::new(TokenType::RightBrace, 1),
        ];
        assert_eq!(expected_token_stream, actual_token_stream);
    }

    #[test]
    fn test_scanner_breaks_up_on_newline() {
        let input = r#"
            var abc = 123
            456;
        "#.trim();
        let actual_token_stream = Scanner::new(input).scan().unwrap();
        let expected_token_stream = vec![
            Token::new(TokenType::Var, 1),
            Token::new(TokenType::Identifier("abc".to_string()), 1),
            Token::new(TokenType::Equal, 1),
            Token::new(TokenType::Number(123.0), 1),
            Token::new(TokenType::Number(456.0), 2),
            Token::new(TokenType::Semicolon, 2),
        ];
        assert_eq!(expected_token_stream, actual_token_stream);
    }

    #[test]
    fn test_identifiers_with_numbers_are_identifiers() {
        let input = r#"
            var abc1 = 123
            var abc42def = 123
        "#.trim();
        let actual_token_stream = Scanner::new(input).scan().unwrap();
        let expected_token_stream = vec![
            Token::new(TokenType::Var, 1),
            Token::new(TokenType::Identifier("abc1".to_string()), 1),
            Token::new(TokenType::Equal, 1),
            Token::new(TokenType::Number(123.0), 1),
            Token::new(TokenType::Var, 2),
            Token::new(TokenType::Identifier("abc42def".to_string()), 2),
            Token::new(TokenType::Equal, 2),
            Token::new(TokenType::Number(123.0), 2),
        ];
        assert_eq!(expected_token_stream, actual_token_stream);
    }

    #[test]
    fn test_identifiers_between_non_whitespace_are_identifiers() {
        let input = r#"
            var abc1=123
            var abc42def=123
        "#.trim();
        let actual_token_stream = Scanner::new(input).scan().unwrap();
        let expected_token_stream = vec![
            Token::new(TokenType::Var, 1),
            Token::new(TokenType::Identifier("abc1".to_string()), 1),
            Token::new(TokenType::Equal, 1),
            Token::new(TokenType::Number(123.0), 1),
            Token::new(TokenType::Var, 2),
            Token::new(TokenType::Identifier("abc42def".to_string()), 2),
            Token::new(TokenType::Equal, 2),
            Token::new(TokenType::Number(123.0), 2),
        ];
        assert_eq!(expected_token_stream, actual_token_stream);
    }

    #[test]
    fn test_escape_sequences_in_string_literals() {
        let input = r#"
            "\"" "\\" "\"in quotes\"" "text \"in quotes\" can be wrapped with \\\""
        "#.trim();
        let actual_token_stream = Scanner::new(input).scan().unwrap();
        let expected_token_stream = vec![
            Token::new(TokenType::StringLiteral("\"".to_string()), 1),
            Token::new(TokenType::StringLiteral("\\".to_string()), 1),
            Token::new(TokenType::StringLiteral("\"in quotes\"".to_string()), 1),
            Token::new(TokenType::StringLiteral("text \"in quotes\" can be wrapped with \\\"".to_string()), 1),
        ];
        assert_eq!(expected_token_stream, actual_token_stream);
    }
}
