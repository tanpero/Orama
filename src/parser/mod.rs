pub mod expr_parser;
pub mod stmt_parser;
pub mod type_parser;

use crate::ast::{Program, Stmt};
use crate::token::{Token, TokenType};
use thiserror::Error;

#[derive(Error, Debug)]
pub enum ParseError {
    #[error("Unexpected token: expected {expected}, found {found} at line {line}, column {column}")]
    UnexpectedToken {
        expected: String,
        found: String,
        line: usize,
        column: usize,
    },
    #[error("Unexpected end of input")]
    UnexpectedEOF,
    #[error("Invalid syntax: {0} at line {1}, column {2}")]
    InvalidSyntax(String, usize, usize),
}

pub struct Parser {
    tokens: Vec<Token>,
    current: usize,
}

// 在Parser实现中添加这些方法
impl Parser {
    pub fn new(tokens: Vec<Token>) -> Self {
        Parser { tokens, current: 0 }
    }

    pub fn parse(&mut self) -> Result<Program, ParseError> {
        let mut statements = Vec::new();

        while !self.is_at_end() {
            statements.push(self.statement()?);
        }

        Ok(Program { statements })
    }

    fn statement(&mut self) -> Result<Stmt, ParseError> {
        if self.match_token(&[TokenType::Let]) {
            self.variable_declaration()
        } else if self.match_token(&[TokenType::Effect]) {
            self.effect_declaration()
        } else if self.match_token(&[TokenType::Type]) {
            self.type_declaration()
        } else {
            let expr = self.expression()?;
            Ok(Stmt::Expression(expr))
        }
    }

    // 这些方法将在子模块中实现
    fn variable_declaration(&mut self) -> Result<Stmt, ParseError> {
        stmt_parser::parse_variable_declaration(self)
    }

    fn effect_declaration(&mut self) -> Result<Stmt, ParseError> {
        stmt_parser::parse_effect_declaration(self)
    }

    fn type_declaration(&mut self) -> Result<Stmt, ParseError> {
        stmt_parser::parse_type_declaration(self)
    }

    fn expression(&mut self) -> Result<crate::ast::Expr, ParseError> {
        expr_parser::parse_expression(self)
    }

    // 辅助方法
    pub fn peek(&self) -> &Token {
        &self.tokens[self.current]
    }

    pub fn previous(&self) -> &Token {
        &self.tokens[self.current - 1]
    }

    pub fn advance(&mut self) -> &Token {
        if !self.is_at_end() {
            self.current += 1;
        }
        self.previous()
    }

    pub fn check(&self, token_type: &TokenType) -> bool {
        if self.is_at_end() {
            return false;
        }
        &self.peek().token_type == token_type
    }

    pub fn check_next(&self, token_type: &TokenType) -> bool {
        if self.current + 1 >= self.tokens.len() {
            return false;
        }
        &self.tokens[self.current + 1].token_type == token_type
    }

    pub fn check_ahead(&self, n: usize, token_type: &TokenType) -> bool {
        if self.current + n >= self.tokens.len() {
            return false;
        }
        &self.tokens[self.current + n].token_type == token_type
    }

    pub fn match_token(&mut self, types: &[TokenType]) -> bool {
        for token_type in types {
            if self.check(token_type) {
                self.advance();
                return true;
            }
        }
        false
    }

    pub fn consume(&mut self, token_type: TokenType, message: &str) -> Result<&Token, ParseError> {
        if self.check(&token_type) {
            Ok(self.advance())
        } else {
            let token = self.peek();
            Err(ParseError::UnexpectedToken {
                expected: format!("{:?}", token_type),
                found: format!("{:?}", token.token_type),
                line: token.line,
                column: token.column,
            })
        }
    }

    pub fn is_at_end(&self) -> bool {
        matches!(self.peek().token_type, TokenType::EOF)
    }

    pub fn synchronize(&mut self) {
        self.advance();

        while !self.is_at_end() {
            if matches!(self.previous().token_type, TokenType::RightBrace) {
                return;
            }

            match self.peek().token_type {
                TokenType::Let | TokenType::If | TokenType::Effect | TokenType::Type => {
                    return;
                }
                _ => {}
            }

            self.advance();
        }
    }
}

pub fn parse(tokens: Vec<Token>) -> Result<Program, ParseError> {
    let mut parser = Parser::new(tokens);
    parser.parse()
}
