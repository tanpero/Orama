use crate::ast::{Expr, MatchCase, Pattern};
use crate::parser::expr_parser::{parse_block_contents, parse_expression};
use crate::parser::{ParseError, Parser};
use crate::token::TokenType;

pub fn parse_if_expression(parser: &mut Parser) -> Result<Expr, ParseError> {
    let condition = parse_expression(parser)?;

    // 解析 then 分支
    parser.consume(TokenType::LeftBrace, "Expect '{' after if condition")?;

    // 修改这里，解析块内容
    let (then_stmts, then_expr) = parse_block_contents(parser)?;

    // 解析可选的 else 分支
    let else_branch = if parser.match_token(&[TokenType::Else]) {
        if parser.match_token(&[TokenType::If]) {
            // 嵌套的 if
            Some(Box::new(parse_if_expression(parser)?))
        } else {
            // else 块
            parser.consume(TokenType::LeftBrace, "Expect '{' after else")?;
            let (else_stmts, else_expr) = parse_block_contents(parser)?;
            Some(Box::new(Expr::Block(else_stmts, else_expr)))
        }
    } else {
        None
    };

    Ok(Expr::If(
        Box::new(condition),
        Box::new(Expr::Block(then_stmts, then_expr)),
        else_branch,
    ))
}

pub fn parse_match_expression(parser: &mut Parser) -> Result<Expr, ParseError> {
    let value = parse_expression(parser)?;

    parser.consume(TokenType::LeftBrace, "Expect '{' after match value")?;

    let mut cases = Vec::new();

    while !parser.check(&TokenType::RightBrace) && !parser.is_at_end() {
        let pattern = parse_pattern(parser)?;

        parser.consume(TokenType::Arrow, "Expect '=>' after pattern")?;

        let body = parse_expression(parser)?;

        cases.push(MatchCase { pattern, body });

        // 如果有逗号，消耗它
        parser.match_token(&[TokenType::Comma]);
    }

    parser.consume(TokenType::RightBrace, "Expect '}' after match cases")?;

    Ok(Expr::Match(Box::new(value), cases))
}

pub fn parse_pattern(parser: &mut Parser) -> Result<Pattern, ParseError> {
    if let TokenType::Identifier(name) = &parser.peek().token_type {
        let name = name.clone();
        parser.advance();

        // 检查是否有参数列表
        if parser.match_token(&[TokenType::LeftParen]) {
            let mut params = Vec::new();

            // 解析参数
            if !parser.check(&TokenType::RightParen) {
                loop {
                    let param = parse_pattern(parser)?;
                    params.push(param);

                    if !parser.match_token(&[TokenType::Comma]) {
                        break;
                    }
                }
            }

            parser.consume(TokenType::RightParen, "Expect ')' after pattern parameters")?;

            Ok(Pattern {
                name,
                params: Some(params),
            })
        } else {
            // 没有参数的简单模式
            Ok(Pattern { name, params: None })
        }
    } else {
        Err(ParseError::UnexpectedToken {
            expected: "pattern".to_string(),
            found: format!("{:?}", parser.peek().token_type),
            line: parser.peek().line,
            column: parser.peek().column,
        })
    }
}
