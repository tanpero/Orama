use crate::ast::{BinaryOp, Expr, Literal};
use crate::parser::{ParseError, Parser};
use crate::token::TokenType;
use crate::typechecker;
use super::parse_expression;

pub fn parse_array_literal(parser: &mut Parser) -> Result<Expr, ParseError> {
    let mut elements = Vec::new();

    if !parser.check(&TokenType::RightBracket) {
        loop {
            elements.push(parse_expression(parser)?);

            if !parser.match_token(&[TokenType::Comma]) {
                break;
            }
        }
    }

    parser.consume(TokenType::RightBracket, "Expect ']' after array elements")?;

    // 进行类型检查
    let array_literal = Literal::Array(elements.clone());
    match typechecker::check_literal(&array_literal) {
        Ok(_) => Ok(Expr::Literal(array_literal)),
        Err(err) => Err(ParseError::InvalidSyntax(
            format!("数组类型检查错误: {}", err),
            parser.previous().line,
            parser.previous().column,
        )),
    }
}

pub fn parse_object_literal(parser: &mut Parser) -> Result<Expr, ParseError> {
    let mut fields = Vec::new();

    if !parser.check(&TokenType::RightBrace) {
        loop {
            // 解析键（支持字符串字面量和标识符）
            let key = if let TokenType::String(s) = &parser.peek().token_type {
                let s = s.clone();
                parser.advance();
                s
            } else if let TokenType::Identifier(name) = &parser.peek().token_type {
                let name = name.clone();
                parser.advance();
                name
            } else {
                return Err(ParseError::UnexpectedToken {
                    expected: "string or identifier".to_string(),
                    found: format!("{:?}", parser.peek().token_type),
                    line: parser.peek().line,
                    column: parser.peek().column,
                });
            };

            // 解析冒号
            parser.consume(TokenType::Colon, "Expect ':' after object key")?;

            // 解析值
            let value = parse_expression(parser)?;

            fields.push((key, value));

            if !parser.match_token(&[TokenType::Comma]) {
                break;
            }
        }
    }

    parser.consume(TokenType::RightBrace, "Expect '}' after object literal")?;

    Ok(Expr::Literal(Literal::Object(fields)))
}

pub fn parse_index_access(parser: &mut Parser, expr: Expr) -> Result<Expr, ParseError> {
    // 解析索引表达式
    let index = parse_expression(parser)?;

    // 消费右方括号
    parser.consume(TokenType::RightBracket, "Expect ']' after index")?;

    // 创建索引访问表达式（使用二元操作符实现）
    Ok(Expr::Binary(
        Box::new(expr),
        BinaryOp::Index,
        Box::new(index),
    ))
}
