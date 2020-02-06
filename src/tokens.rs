use std::error;
use std::fmt::Formatter;
use std::fmt;
use std::str::FromStr;
use std::string::ParseError;


#[derive(Debug, PartialEq)]
pub enum TokenType {
    // single character tokens
    LeftParen, RightParen, LeftBrace, RightBrace,
    Comma, Dot, Minus,
    Plus, Semicolon, Slash, Star,

    // one or two character tokens
    Bang, BangEqual, Equal, EqualEqual, Greater,
    GreaterEqual, Less, LessEqual,

    // literals
    Identifier(String),
    StringLiteral(String),
    Number(f64),

    // keywords
    And, Class, Else, False, Fun, For,
    If, Nil, Or, Print, Return, Super, This,
    True, Var, While,

    Eof,
}
impl fmt::Display for TokenType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", match self {
            TokenType::LeftParen            => "(",
            TokenType::RightParen           => ")",
            TokenType::LeftBrace            => "{",
            TokenType::RightBrace           => "}",
            TokenType::Identifier(ident)    => ident,
            _ => "",
        }.to_string())
    }
}
#[derive(Debug)]
pub struct ParseTokenError(pub String);
// TODO implement Display, and Error (Error is Debug + Display)
impl fmt::Display for ParseTokenError {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}
impl error::Error for ParseTokenError {}

#[derive(Debug, PartialEq)]
pub struct Token {
    pub token_type: TokenType,
    pub line_number: usize,
}
impl Token {
    pub fn new(token_type: TokenType, line_number: usize) -> Self {
        Token {
            token_type,
            line_number,
        }
    }
}
impl fmt::Display for Token {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{:?} {}", self.token_type, self.token_type)
    }
}