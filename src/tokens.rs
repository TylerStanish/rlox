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
            TokenType::LeftParen            => "(".to_string(),
            TokenType::RightParen           => ")".to_string(),
            TokenType::LeftBrace            => "{".to_string(),
            TokenType::RightBrace           => "}".to_string(),
            TokenType::Comma                => ",".to_string(),
            TokenType::Dot                  => ".".to_string(),
            TokenType::Minus                => "-".to_string(),
            TokenType::Plus                 => "+".to_string(),
            TokenType::Semicolon            => ";".to_string(),
            TokenType::Slash                => "/".to_string(),
            TokenType::Star                 => "*".to_string(),
            TokenType::Bang                 => "!".to_string(),
            TokenType::BangEqual            => "!=".to_string(),
            TokenType::Equal                => "=".to_string(),
            TokenType::EqualEqual           => "==".to_string(),
            TokenType::Greater              => ">".to_string(),
            TokenType::GreaterEqual         => ">=".to_string(),
            TokenType::Less                 => "<".to_string(),
            TokenType::LessEqual            => "<=".to_string(),
            TokenType::And                  => "and".to_string(),
            TokenType::Class                => "class".to_string(),
            TokenType::Else                 => "else".to_string(),
            TokenType::False                => "false".to_string(),
            TokenType::Fun                  => "fun".to_string(),
            TokenType::For                  => "for".to_string(),
            TokenType::If                   => "if".to_string(),
            TokenType::Nil                  => "nil".to_string(),
            TokenType::Or                   => "or".to_string(),
            TokenType::Print                => "print".to_string(),
            TokenType::Return               => "return".to_string(),
            TokenType::Super                => "super".to_string(),
            TokenType::This                 => "this".to_string(),
            TokenType::True                 => "true".to_string(),
            TokenType::Var                  => "var".to_string(),
            TokenType::While                => "while".to_string(),
            TokenType::Eof                  => "EOF".to_string(),
            TokenType::Identifier(ident)    => ident.clone(),
            TokenType::StringLiteral(lit)   => format!("\"{}\"", lit.clone()),
            TokenType::Number(num)          => num.to_string(),
        })
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
        write!(f, "Token{{{:?} {}}}", self.token_type, self.line_number)
    }
}