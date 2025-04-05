use crate::ast::{Expr, EffectHandler, EffectOperation, ReturnHandler, Parameter};
use crate::parser::{ParseError, Parser};
use crate::token::TokenType;
use super::parse_expression;

pub fn parse_perform(parser: &mut Parser) -> Result<Expr, ParseError> {
    let effect_name = if let TokenType::Identifier(name) = &parser.peek().token_type {
        name.clone()
    } else {
        return Err(ParseError::UnexpectedToken {
            expected: "effect name".to_string(),
            found: format!("{:?}", parser.peek().token_type),
            line: parser.peek().line,
            column: parser.peek().column,
        });
    };
    parser.advance();

    parser.consume(TokenType::Dot, "Expect '.' after effect name")?;

    let operation_name = if let TokenType::Identifier(name) = &parser.peek().token_type {
        name.clone()
    } else {
        return Err(ParseError::UnexpectedToken {
            expected: "operation name".to_string(),
            found: format!("{:?}", parser.peek().token_type),
            line: parser.peek().line,
            column: parser.peek().column,
        });
    };
    parser.advance();

    parser.consume(TokenType::LeftParen, "Expect '(' after operation name")?;

    let mut arguments = Vec::new();
    if !parser.check(&TokenType::RightParen) {
        loop {
            arguments.push(parse_expression(parser)?);
            if !parser.match_token(&[TokenType::Comma]) {
                break;
            }
        }
    }

    parser.consume(TokenType::RightParen, "Expect ')' after arguments")?;

    Ok(Expr::Perform(effect_name, operation_name, arguments))
}

pub fn parse_handle(parser: &mut Parser) -> Result<Expr, ParseError> {
    // 解析被处理的表达式
    let expr = parse_expression(parser)?;

    parser.consume(
        TokenType::LeftBrace,
        "Expect '{' after expression in handle",
    )?;

    // 解析效应处理器
    let mut handlers = Vec::new();
    let mut return_handler = None;

    while !parser.check(&TokenType::RightBrace) && !parser.is_at_end() {
        if parser.match_token(&[TokenType::Effect]) {
            // 解析效应处理器
            let effect_name = if let TokenType::Identifier(name) = &parser.peek().token_type {
                name.clone()
            } else {
                return Err(ParseError::UnexpectedToken {
                    expected: "effect name".to_string(),
                    found: format!("{:?}", parser.peek().token_type),
                    line: parser.peek().line,
                    column: parser.peek().column,
                });
            };
            parser.advance();

            parser.consume(TokenType::LeftBrace, "Expect '{' after effect name")?;

            let mut operations = Vec::new();

            while !parser.check(&TokenType::RightBrace) && !parser.is_at_end() {
                // 解析操作名
                let op_name = if let TokenType::Identifier(name) = &parser.peek().token_type {
                    name.clone()
                } else {
                    return Err(ParseError::UnexpectedToken {
                        expected: "operation name".to_string(),
                        found: format!("{:?}", parser.peek().token_type),
                        line: parser.peek().line,
                        column: parser.peek().column,
                    });
                };
                parser.advance();

                parser.consume(TokenType::Colon, "Expect ':' after operation name")?;
                parser.consume(TokenType::LeftParen, "Expect '(' after ':'")?;

                // 解析参数
                let mut params = Vec::new();
                if !parser.check(&TokenType::RightParen) {
                    loop {
                        let param_name =
                            if let TokenType::Identifier(name) = &parser.peek().token_type {
                                name.clone()
                            } else {
                                return Err(ParseError::UnexpectedToken {
                                    expected: "parameter name".to_string(),
                                    found: format!("{:?}", parser.peek().token_type),
                                    line: parser.peek().line,
                                    column: parser.peek().column,
                                });
                            };
                        parser.advance();

                        // 这里可以添加对类型注解的支持
                        let param = Parameter {
                            name: param_name,
                            type_annotation: None,
                        };
                        params.push(param);

                        if !parser.match_token(&[TokenType::Comma]) {
                            break;
                        }
                    }
                }

                parser.consume(TokenType::RightParen, "Expect ')' after parameters")?;
                parser.consume(TokenType::FatArrow, "Expect '=>' after parameters")?;

                // 解析操作体
                let body = parse_expression(parser)?;

                operations.push(EffectOperation {
                    name: op_name,
                    params,
                    body,
                });

                // 如果有逗号，消费它
                parser.match_token(&[TokenType::Comma]);
            }

            parser.consume(TokenType::RightBrace, "Expect '}' after effect operations")?;

            handlers.push(EffectHandler {
                effect_name,
                operations,
            });
        } else if parser.match_token(&[TokenType::Return]) {
            // 解析返回处理器
            parser.consume(TokenType::Colon, "Expect ':' after 'return'")?;
            parser.consume(TokenType::LeftParen, "Expect '(' after ':'")?;

            let param_name = if let TokenType::Identifier(name) = &parser.peek().token_type {
                name.clone()
            } else {
                return Err(ParseError::UnexpectedToken {
                    expected: "parameter name".to_string(),
                    found: format!("{:?}", parser.peek().token_type),
                    line: parser.peek().line,
                    column: parser.peek().column,
                });
            };
            parser.advance();

            parser.consume(TokenType::RightParen, "Expect ')' after parameter")?;
            parser.consume(TokenType::FatArrow, "Expect '=>' after parameter")?;

            let body = parse_expression(parser)?;

            return_handler = Some(Box::new(ReturnHandler {
                param: param_name,
                body: Box::new(body),
            }));

            // 如果有逗号，消费它
            parser.match_token(&[TokenType::Comma]);
        } else {
            return Err(ParseError::UnexpectedToken {
                expected: "'effect' or 'return'".to_string(),
                found: format!("{:?}", parser.peek().token_type),
                line: parser.peek().line,
                column: parser.peek().column,
            });
        }
    }

    parser.consume(TokenType::RightBrace, "Expect '}' after handle body")?;

    Ok(Expr::Handle(Box::new(expr), handlers, return_handler))
}