use crate::ast::{Expr, Parameter};
use crate::parser::{ParseError, Parser};
use crate::token::TokenType;
use crate::parser::expr_parser::{parse_expression, parse_block_contents};
use crate::parser::type_parser::parse_type_annotation;
use crate::ast::Literal;

pub fn parse_empty_param_function(parser: &mut Parser) -> Result<Expr, ParseError> {
    // 解析空参数函数
    parser.advance(); // 消费左括号
    parser.advance(); // 消费右括号

    if parser.match_token(&[TokenType::FatArrow]) {
        // 箭头函数
        // 检查是否是块函数体
        if parser.check(&TokenType::LeftBrace) && !parser.check_ahead(1, &TokenType::Colon) {
            // 是块函数体，不是对象字面量
            parser.advance(); // 消费左大括号
            let (stmts, expr) = parse_block_contents(parser)?;
            return Ok(Expr::Function(Vec::new(), Box::new(Expr::Block(stmts, expr))));
        } else {
            // 普通表达式函数体
            let body = parse_expression(parser)?;
            return Ok(Expr::Function(Vec::new(), Box::new(body)));
        }
    } else if parser.match_token(&[TokenType::LeftBrace]) {
        // 块函数
        let (stmts, expr) = parse_block_contents(parser)?;
        return Ok(Expr::Function(
            Vec::new(),
            Box::new(Expr::Block(stmts, expr)),
        ));
    }

    Err(ParseError::UnexpectedToken {
        expected: "'=>' or '{'".to_string(),
        found: format!("{:?}", parser.peek().token_type),
        line: parser.peek().line,
        column: parser.peek().column,
    })
}

pub fn parse_parenthesized_expr_or_function(parser: &mut Parser) -> Result<Expr, ParseError> {
    if parser.match_token(&[TokenType::RightParen]) {
        // 空参数列表的函数
        if parser.match_token(&[TokenType::FatArrow]) {
            // 检查是否是块函数体
            if parser.check(&TokenType::LeftBrace) && !parser.check_ahead(1, &TokenType::Colon) {
                // 是块函数体，不是对象字面量
                parser.advance(); // 消费左大括号
                let (stmts, expr) = parse_block_contents(parser)?;
                return Ok(Expr::Function(Vec::new(), Box::new(Expr::Block(stmts, expr))));
            } else {
                // 普通表达式函数体
                let body = parse_expression(parser)?;
                return Ok(Expr::Function(Vec::new(), Box::new(body)));
            }
        } else if parser.match_token(&[TokenType::LeftBrace]) {
            // 支持块状函数体（空参数情况）
            let (stmts, expr) = parse_block_contents(parser)?;
            return Ok(Expr::Function(
                Vec::new(),
                Box::new(Expr::Block(stmts, expr)),
            ));
        }
        return Ok(Expr::Literal(Literal::Unit)); // 返回单元类型
    } else {
        // 修改这里，支持多参数函数定义
        let mut params = Vec::new();

        // 解析第一个参数
        if let TokenType::Identifier(name) = &parser.peek().token_type {
            let name = name.clone();
            parser.advance();

            // 解析可选的类型注解
            let type_annotation = if parser.match_token(&[TokenType::Colon]) {
                Some(parse_type_annotation(parser)?)
            } else {
                None
            };

            let param = Parameter {
                name,
                type_annotation,
            };
            params.push(param);

            // 解析后续参数
            while parser.match_token(&[TokenType::Comma]) {
                if let TokenType::Identifier(name) = &parser.peek().token_type {
                    let name = name.clone();
                    parser.advance();

                    // 解析可选的类型注解
                    let type_annotation = if parser.match_token(&[TokenType::Colon]) {
                        Some(parse_type_annotation(parser)?)
                    } else {
                        None
                    };

                    let param = Parameter {
                        name,
                        type_annotation,
                    };
                    params.push(param);
                } else {
                    return Err(ParseError::UnexpectedToken {
                        expected: "parameter name".to_string(),
                        found: format!("{:?}", parser.peek().token_type),
                        line: parser.peek().line,
                        column: parser.peek().column,
                    });
                }
            }

            parser.consume(TokenType::RightParen, "Expect ')' after parameters")?;

            // 解析可选的返回类型注解
            let _return_type_annotation = if parser.match_token(&[TokenType::Colon]) {
                Some(parse_type_annotation(parser)?)
            } else {
                None
            };

            if parser.match_token(&[TokenType::FatArrow]) {
                let body = parse_expression(parser)?;
                return Ok(Expr::Function(params, Box::new(body)));
            } else if parser.match_token(&[TokenType::LeftBrace]) {
                // 支持块状函数体
                let (stmts, expr) = parse_block_contents(parser)?;
                return Ok(Expr::Function(params, Box::new(Expr::Block(stmts, expr))));
            } else {
                // 不是函数定义，回退到表达式解析
                return Err(ParseError::UnexpectedToken {
                    expected: "'=>' or '{'".to_string(),
                    found: format!("{:?}", parser.peek().token_type),
                    line: parser.peek().line,
                    column: parser.peek().column,
                });
            }
        } else {
            // 不是参数名，可能是分组表达式
            let expr = parse_expression(parser)?;
            parser.consume(TokenType::RightParen, "Expect ')' after expression")?;

            if parser.match_token(&[TokenType::FatArrow]) {
                // 尝试将表达式作为单参数处理
                if let Expr::Variable(name) = expr {
                    let param = Parameter {
                        name,
                        type_annotation: None,
                    };
                    let body = parse_expression(parser)?;
                    return Ok(Expr::Function(vec![param], Box::new(body)));
                } else {
                    return Err(ParseError::InvalidSyntax(
                        "Expected parameter name".to_string(),
                        parser.previous().line,
                        parser.previous().column,
                    ));
                }
            } else if parser.match_token(&[TokenType::LeftBrace])
                && matches!(expr, Expr::Variable(_))
            {
                // 支持块状函数体（单参数情况）
                if let Expr::Variable(name) = expr {
                    let param = Parameter {
                        name,
                        type_annotation: None,
                    };
                    let (stmts, expr) = parse_block_contents(parser)?;
                    return Ok(Expr::Function(
                        vec![param],
                        Box::new(Expr::Block(stmts, expr)),
                    ));
                }
            }
            return Ok(expr);
        }
    }
}

pub fn parse_block_expression(parser: &mut Parser) -> Result<Expr, ParseError> {
    parser.consume(
        TokenType::LeftBrace,
        "Expect '{' at the beginning of a block",
    )?;

    let (statements, expr) = parse_block_contents(parser)?;

    Ok(Expr::Block(statements, expr))
}
