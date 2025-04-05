use std::fmt;

#[derive(Debug, PartialEq, Clone)]
pub enum TokenType {
    // 关键字
    Let,
    If,
    Else,
    Effect,
    Type,
    Match,
    Perform,
    Handle,
    Return,
    True,
    False,
    Null,

    // 标识符和字面量
    Identifier(String),
    Number(f64),
    String(String),

    // 运算符
    Plus,
    Minus,
    Asterisk,
    Slash,
    Percent,
    Equal,
    EqualEqual,
    NotEqual,
    Less,
    Greater,
    LessEqual,
    GreaterEqual,
    And,
    Or,
    Not,

    // 分隔符
    LeftParen,
    RightParen,
    LeftBrace,
    RightBrace,
    LeftBracket,
    RightBracket,
    Comma,
    Dot,
    Colon,
    Arrow,
    FatArrow,
    Pipe,
    PipeArrow,
    LessThan,
    GreaterThan,

    // 其他
    EOF,
}

impl fmt::Display for TokenType {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            TokenType::Let => write!(f, "Let"),
            TokenType::If => write!(f, "If"),
            TokenType::Else => write!(f, "Else"),
            TokenType::Effect => write!(f, "Effect"),
            TokenType::Type => write!(f, "Type"),
            TokenType::Match => write!(f, "Match"),
            TokenType::Perform => write!(f, "Perform"),
            TokenType::Handle => write!(f, "Handle"),
            TokenType::Return => write!(f, "Return"),
            TokenType::True => write!(f, "True"),
            TokenType::False => write!(f, "False"),
            TokenType::Null => write!(f, "Null"),
            TokenType::Identifier(s) => write!(f, "Identifier({})", s),
            TokenType::Number(n) => write!(f, "Number({})", n),
            TokenType::String(s) => write!(f, "String(\"{}\")", s),
            TokenType::Plus => write!(f, "Plus"),
            TokenType::Minus => write!(f, "Minus"),
            TokenType::Asterisk => write!(f, "Asterisk"),
            TokenType::Slash => write!(f, "Slash"),
            TokenType::Percent => write!(f, "Percent"),
            TokenType::Equal => write!(f, "Equal"),
            TokenType::EqualEqual => write!(f, "EqualEqual"),
            TokenType::NotEqual => write!(f, "NotEqual"),
            TokenType::Less => write!(f, "Less"),
            TokenType::Greater => write!(f, "Greater"),
            TokenType::LessEqual => write!(f, "LessEqual"),
            TokenType::GreaterEqual => write!(f, "GreaterEqual"),
            TokenType::And => write!(f, "And"),
            TokenType::Or => write!(f, "Or"),
            TokenType::Not => write!(f, "Not"),
            TokenType::LeftParen => write!(f, "LeftParen"),
            TokenType::RightParen => write!(f, "RightParen"),
            TokenType::LeftBrace => write!(f, "LeftBrace"),
            TokenType::RightBrace => write!(f, "RightBrace"),
            TokenType::LeftBracket => write!(f, "LeftBracket"),
            TokenType::RightBracket => write!(f, "RightBracket"),
            TokenType::Comma => write!(f, "Comma"),
            TokenType::Dot => write!(f, "Dot"),
            TokenType::Colon => write!(f, "Colon"),
            TokenType::Arrow => write!(f, "Arrow"),
            TokenType::FatArrow => write!(f, "FatArrow"),
            TokenType::Pipe => write!(f, "Pipe"),
            TokenType::PipeArrow => write!(f, "PipeArrow"),
            TokenType::LessThan => write!(f, "LessThan"),
            TokenType::GreaterThan => write!(f, "GreaterThan"),
            TokenType::EOF => write!(f, "EOF"),
        }
    }
}

#[derive(Debug, Clone)]
pub struct Token {
    pub token_type: TokenType,
    pub lexeme: String,
    pub line: usize,
    pub column: usize,
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "{} '{}' at {}:{}",
            self.token_type, self.lexeme, self.line, self.column
        )
    }
}
