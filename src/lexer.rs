use crate::token::{Token, TokenType};
use thiserror::Error;

#[derive(Error, Debug)]
pub enum LexerError {
    #[error("Unexpected character: '{0}' at line {1}, column {2}")]
    UnexpectedCharacter(char, usize, usize),
    #[error("Unterminated string at line {0}, column {1}")]
    UnterminatedString(usize, usize),
    #[error("Invalid escape sequence: '\\{0}' at line {1}, column {2}")]
    InvalidEscapeSequence(char, usize, usize),
}

pub struct Lexer {
    source: Vec<char>,
    tokens: Vec<Token>,
    start: usize,
    current: usize,
    line: usize,
    column: usize,
}

impl Lexer {
    pub fn new(source: &str) -> Self {
        Lexer {
            source: source.chars().collect(),
            tokens: Vec::new(),
            start: 0,
            current: 0,
            line: 1,
            column: 1,
        }
    }

    pub fn scan_tokens(&mut self) -> Result<Vec<Token>, LexerError> {
        while !self.is_at_end() {
            self.start = self.current;
            self.scan_token()?;
        }

        self.tokens.push(Token {
            token_type: TokenType::EOF,
            lexeme: String::from(""),
            line: self.line,
            column: self.column,
        });

        Ok(self.tokens.clone())
    }

    fn scan_token(&mut self) -> Result<(), LexerError> {
        let c = self.advance();

        match c {
            // 单字符标记
            '(' => self.add_token(TokenType::LeftParen),
            ')' => self.add_token(TokenType::RightParen),
            '{' => self.add_token(TokenType::LeftBrace),
            '}' => self.add_token(TokenType::RightBrace),
            '[' => self.add_token(TokenType::LeftBracket),
            ']' => self.add_token(TokenType::RightBracket),
            ',' => self.add_token(TokenType::Comma),
            '.' => self.add_token(TokenType::Dot),
            ':' => self.add_token(TokenType::Colon),
            '+' => self.add_token(TokenType::Plus),
            '*' => self.add_token(TokenType::Asterisk),
            '%' => self.add_token(TokenType::Percent),
            '&' => {
                if self.match_char('&') {
                    self.add_token(TokenType::LogicalAnd);
                } else {
                    self.add_token(TokenType::And);
                }
            }
            '|' => {
                if self.match_char('|') {
                    self.add_token(TokenType::LogicalOr);
                } else if self.match_char('>') {
                    self.add_token(TokenType::PipeArrow);
                } else {
                    self.add_token(TokenType::Pipe);
                }
            }

            // 可能是一个或两个字符的标记
            '-' => {
                if self.match_char('>') {
                    self.add_token(TokenType::Arrow);
                } else {
                    self.add_token(TokenType::Minus);
                }
            }
            '=' => {
                if self.match_char('=') {
                    self.add_token(TokenType::EqualEqual);
                } else if self.match_char('>') {
                    self.add_token(TokenType::FatArrow);
                } else {
                    self.add_token(TokenType::Equal);
                }
            }
            '!' => {
                if self.match_char('=') {
                    self.add_token(TokenType::NotEqual);
                } else {
                    self.add_token(TokenType::Not);
                }
            }
            '<' => {
                if self.match_char('=') {
                    self.add_token(TokenType::LessEqual);
                } else {
                    self.add_token(TokenType::LessThan);
                }
            }
            '>' => {
                if self.match_char('=') {
                    self.add_token(TokenType::GreaterEqual);
                } else {
                    self.add_token(TokenType::GreaterThan);
                }
            }
            '/' => {
                if self.match_char('/') {
                    // 单行注释
                    while self.peek() != '\n' && !self.is_at_end() {
                        self.advance();
                    }
                } else if self.match_char('*') {
                    // 多行注释
                    self.scan_multiline_comment()?;
                } else {
                    self.add_token(TokenType::Slash);
                }
            }

            // 忽略空白字符
            ' ' | '\r' | '\t' => {}
            '\n' => {
                self.line += 1;
                self.column = 1;
            }

            // 字符串
            '"' => self.string()?,

            // 数字和标识符
            c if c.is_digit(10) => self.number(),
            c if c.is_alphabetic() || c == '_' => self.identifier(),

            // 未知字符
            c => {
                return Err(LexerError::UnexpectedCharacter(
                    c,
                    self.line,
                    self.column - 1,
                ));
            }
        }

        Ok(())
    }

    fn scan_multiline_comment(&mut self) -> Result<(), LexerError> {
        let mut nesting = 1;

        while nesting > 0 && !self.is_at_end() {
            if self.peek() == '/' && self.peek_next() == '*' {
                self.advance();
                self.advance();
                nesting += 1;
            } else if self.peek() == '*' && self.peek_next() == '/' {
                self.advance();
                self.advance();
                nesting -= 1;
            } else if self.peek() == '\n' {
                self.advance();
                self.line += 1;
                self.column = 1;
            } else {
                self.advance();
            }
        }

        Ok(())
    }

    fn identifier(&mut self) {
        while self.peek().is_alphanumeric() || self.peek() == '_' {
            self.advance();
        }

        let text = self.source[self.start..self.current]
            .iter()
            .collect::<String>();

        let token_type = match text.as_str() {
            "let" => TokenType::Let,
            "if" => TokenType::If,
            "else" => TokenType::Else,
            "effect" => TokenType::Effect,
            "type" => TokenType::Type,
            "match" => TokenType::Match,
            "perform" => TokenType::Perform,
            "handle" => TokenType::Handle,
            "return" => TokenType::Return,
            "true" => TokenType::True,
            "false" => TokenType::False,
            "null" => TokenType::Null,
            _ => TokenType::Identifier(text.clone()),
        };

        self.add_token(token_type);
    }

    fn number(&mut self) {
        while self.peek().is_digit(10) {
            self.advance();
        }

        // 小数部分
        if self.peek() == '.' && self.peek_next().is_digit(10) {
            // 消费 '.'
            self.advance();

            while self.peek().is_digit(10) {
                self.advance();
            }
        }

        let value = self.source[self.start..self.current]
            .iter()
            .collect::<String>()
            .parse::<f64>()
            .unwrap();

        self.add_token(TokenType::Number(value));
    }

    fn string(&mut self) -> Result<(), LexerError> {
        let start_line = self.line;
        let start_column = self.column - 1;
        let mut value = String::new();

        while self.peek() != '"' && !self.is_at_end() {
            if self.peek() == '\n' {
                self.line += 1;
                self.column = 1;
            } else if self.peek() == '\\' {
                self.advance(); // 消费 '\'
                match self.peek() {
                    'n' => value.push('\n'),
                    't' => value.push('\t'),
                    'r' => value.push('\r'),
                    '"' => value.push('"'),
                    '\\' => value.push('\\'),
                    '0' => value.push('\0'),
                    c => return Err(LexerError::InvalidEscapeSequence(c, self.line, self.column)),
                }
            } else {
                value.push(self.peek());
            }
            self.advance();
        }

        if self.is_at_end() {
            return Err(LexerError::UnterminatedString(start_line, start_column));
        }

        // 消费闭合的 '"'
        self.advance();

        self.add_token(TokenType::String(value));
        Ok(())
    }

    fn match_char(&mut self, expected: char) -> bool {
        if self.is_at_end() || self.source[self.current] != expected {
            return false;
        }

        self.current += 1;
        self.column += 1;
        true
    }

    fn peek(&self) -> char {
        if self.is_at_end() {
            '\0'
        } else {
            self.source[self.current]
        }
    }

    fn peek_next(&self) -> char {
        if self.current + 1 >= self.source.len() {
            '\0'
        } else {
            self.source[self.current + 1]
        }
    }

    fn advance(&mut self) -> char {
        let c = self.source[self.current];
        self.current += 1;
        self.column += 1;
        c
    }

    fn add_token(&mut self, token_type: TokenType) {
        let lexeme = self.source[self.start..self.current]
            .iter()
            .collect::<String>();
        self.tokens.push(Token {
            token_type,
            lexeme,
            line: self.line,
            column: self.column - (self.current - self.start),
        });
    }

    fn is_at_end(&self) -> bool {
        self.current >= self.source.len()
    }
}

pub fn lex(source: &str) -> Result<Vec<Token>, LexerError> {
    let mut lexer = Lexer::new(source);
    lexer.scan_tokens()
}
