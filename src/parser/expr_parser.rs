use crate::ast::{BinaryOp, Expr, Literal, Parameter, UnaryOp};
use crate::parser::{ParseError, Parser};
use crate::token::TokenType;

use crate::ast::MatchCase;
use crate::ast::Pattern;
use crate::ast::ReturnHandler;
use crate::ast::EffectHandler;
use crate::ast::EffectOperation;
use crate::typechecker;
use crate::parser::type_parser::parse_type_annotation;
use crate::parser::Stmt;

pub fn parse_expression(parser: &mut Parser) -> Result<Expr, ParseError> {
    parse_pipe(parser)
}

fn parse_pipe(parser: &mut Parser) -> Result<Expr, ParseError> {
    let mut expr = parse_assignment(parser)?;

    while parser.match_token(&[TokenType::PipeArrow]) {
        let right = parse_assignment(parser)?;
        expr = Expr::Pipe(Box::new(expr), Box::new(right));
    }

    Ok(expr)
}

fn parse_assignment(parser: &mut Parser) -> Result<Expr, ParseError> {
    parse_or(parser)
}

fn parse_or(parser: &mut Parser) -> Result<Expr, ParseError> {
    let mut expr = parse_and(parser)?;

    while parser.match_token(&[TokenType::Or]) {
        let right = parse_and(parser)?;
        expr = Expr::Binary(Box::new(expr), BinaryOp::Or, Box::new(right));
    }

    Ok(expr)
}

fn parse_and(parser: &mut Parser) -> Result<Expr, ParseError> {
    let mut expr = parse_equality(parser)?;

    while parser.match_token(&[TokenType::And]) {
        let right = parse_equality(parser)?;
        expr = Expr::Binary(Box::new(expr), BinaryOp::And, Box::new(right));
    }

    Ok(expr)
}

fn parse_equality(parser: &mut Parser) -> Result<Expr, ParseError> {
    let mut expr = parse_comparison(parser)?;

    while parser.match_token(&[TokenType::EqualEqual, TokenType::NotEqual]) {
        let operator = match parser.previous().token_type {
            TokenType::EqualEqual => BinaryOp::Equal,
            TokenType::NotEqual => BinaryOp::NotEqual,
            _ => unreachable!(),
        };
        let right = parse_comparison(parser)?;
        expr = Expr::Binary(Box::new(expr), operator, Box::new(right));
    }

    Ok(expr)
}

fn parse_comparison(parser: &mut Parser) -> Result<Expr, ParseError> {
    let mut expr = parse_term(parser)?;

    while parser.match_token(&[
        TokenType::Less,
        TokenType::Greater,
        TokenType::LessEqual,
        TokenType::GreaterEqual,
        TokenType::LessThan,     // 添加 LessThan 作为 Less 的替代
        TokenType::GreaterThan,  // 添加 GreaterThan 作为 Greater 的替代
    ]) {
        let operator = match parser.previous().token_type {
            TokenType::Less | TokenType::LessThan => BinaryOp::Less,
            TokenType::Greater | TokenType::GreaterThan => BinaryOp::Greater,
            TokenType::LessEqual => BinaryOp::LessEqual,
            TokenType::GreaterEqual => BinaryOp::GreaterEqual,
            _ => unreachable!(),
        };
        let right = parse_term(parser)?;
        expr = Expr::Binary(Box::new(expr), operator, Box::new(right));
    }

    Ok(expr)
}

fn parse_term(parser: &mut Parser) -> Result<Expr, ParseError> {
    let mut expr = parse_factor(parser)?;

    while parser.match_token(&[TokenType::Plus, TokenType::Minus]) {
        let operator = match parser.previous().token_type {
            TokenType::Plus => BinaryOp::Add,
            TokenType::Minus => BinaryOp::Subtract,
            _ => unreachable!(),
        };
        let right = parse_factor(parser)?;
        expr = Expr::Binary(Box::new(expr), operator, Box::new(right));
    }

    Ok(expr)
}

fn parse_factor(parser: &mut Parser) -> Result<Expr, ParseError> {
    let mut expr = parse_unary(parser)?;

    while parser.match_token(&[TokenType::Asterisk, TokenType::Slash, TokenType::Percent]) {
        let operator = match parser.previous().token_type {
            TokenType::Asterisk => BinaryOp::Multiply,
            TokenType::Slash => BinaryOp::Divide,
            TokenType::Percent => BinaryOp::Modulo,
            _ => unreachable!(),
        };
        let right = parse_unary(parser)?;
        expr = Expr::Binary(Box::new(expr), operator, Box::new(right));
    }

    Ok(expr)
}

fn parse_unary(parser: &mut Parser) -> Result<Expr, ParseError> {
    if parser.match_token(&[TokenType::Not, TokenType::Minus]) {
        let operator = match parser.previous().token_type {
            TokenType::Not => UnaryOp::Not,
            TokenType::Minus => UnaryOp::Negate,
            _ => unreachable!(),
        };
        let right = parse_unary(parser)?;
        return Ok(Expr::Unary(operator, Box::new(right)));
    }

    parse_call(parser)
}

fn parse_call(parser: &mut Parser) -> Result<Expr, ParseError> {
    let mut expr = parse_primary(parser)?;
    
    loop {
        if parser.match_token(&[TokenType::LeftParen]) {
            expr = finish_call(parser, expr)?;
        } else if parser.match_token(&[TokenType::LeftBracket]) {
            // 处理索引访问
            expr = parse_index_access(parser, expr)?;
        } else {
            break;
        }
    }
    
    Ok(expr)
}

fn finish_call(parser: &mut Parser, callee: Expr) -> Result<Expr, ParseError> {
    let mut arguments = Vec::new();

    if !parser.check(&TokenType::RightParen) {
        loop {
            arguments.push(parse_expression(parser)?);
            if !parser.match_token(&[TokenType::Comma]) {
                break;
            }
        }
    }

    parser.consume(
        TokenType::RightParen,
        "Expect ')' after arguments",
    )?;

    Ok(Expr::Call(Box::new(callee), arguments))
}

fn parse_array_literal(parser: &mut Parser) -> Result<Expr, ParseError> {
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
        Err(err) => {
            Err(ParseError::InvalidSyntax(
                format!("数组类型检查错误: {}", err),
                parser.previous().line,
                parser.previous().column,
            ))
        }
    }
}

// 在 parse_primary 函数中添加对 match 表达式的支持
fn parse_primary(parser: &mut Parser) -> Result<Expr, ParseError> {
    if parser.match_token(&[TokenType::False]) {
        return Ok(Expr::Literal(Literal::Boolean(false)));
    }
    if parser.match_token(&[TokenType::True]) {
        return Ok(Expr::Literal(Literal::Boolean(true)));
    }
    if parser.match_token(&[TokenType::Null]) {
        return Ok(Expr::Literal(Literal::Null));
    }
    if parser.match_token(&[TokenType::LeftBrace]) {
        return parse_object_literal(parser);
    }
    // 添加对 if 表达式的支持
    if parser.match_token(&[TokenType::If]) {
        return parse_if_expression(parser);
    }

    if parser.match_token(&[TokenType::Match]) {
        return parse_match_expression(parser);
    }

    if let TokenType::Number(n) = parser.peek().token_type {
        parser.advance();
        return Ok(Expr::Literal(Literal::Number(n)));
    }

    if let TokenType::String(s) = &parser.peek().token_type {
        let string = s.clone();
        parser.advance();
        return Ok(Expr::Literal(Literal::String(string)));
    }

    if let TokenType::Identifier(name) = &parser.peek().token_type {
        let name = name.clone();
        parser.advance();
        return Ok(Expr::Variable(name));
    }

    // 添加对数组字面量的支持
    if parser.match_token(&[TokenType::LeftBracket]) {
        return parse_array_literal(parser);
    }

    if parser.match_token(&[TokenType::LeftParen]) {
        if parser.match_token(&[TokenType::RightParen]) {
            // 空参数列表的函数
            if parser.match_token(&[TokenType::FatArrow]) {
                let body = parse_expression(parser)?;
                return Ok(Expr::Function(Vec::new(), Box::new(body)));
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
                let return_type_annotation = if parser.match_token(&[TokenType::Colon]) {
                    Some(parse_type_annotation(parser)?)
                } else {
                    None
                };
                
                if parser.match_token(&[TokenType::FatArrow]) {
                    let body = parse_expression(parser)?;
                    return Ok(Expr::Function(params, Box::new(body)));
                } else {
                    // 不是函数定义，回退到表达式解析
                    return Err(ParseError::UnexpectedToken {
                        expected: "'=>'".to_string(),
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
                }
                
                return Ok(expr);
            }
        }
    }

    // 添加一个 else 子句，处理所有其他情况
    Err(ParseError::UnexpectedToken {
        expected: "expression".to_string(),
        found: format!("{:?}", parser.peek().token_type),
        line: parser.peek().line,
        column: parser.peek().column,
    })
}

fn parse_if_expression(parser: &mut Parser) -> Result<Expr, ParseError> {
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
    
    Ok(Expr::If(Box::new(condition), Box::new(Expr::Block(then_stmts, then_expr)), else_branch))
}

// 新增函数：解析块内容
fn parse_block_contents(parser: &mut Parser) -> Result<(Vec<Stmt>, Option<Box<Expr>>), ParseError> {
    let mut statements = Vec::new();
    
    // 如果块为空，直接返回
    if parser.check(&TokenType::RightBrace) {
        parser.advance(); // 消费右花括号
        return Ok((statements, None));
    }
    
    // 解析块中的语句和表达式
    loop {
        // 检查是否到达块的结尾
        if parser.check(&TokenType::RightBrace) {
            parser.advance(); // 消费右花括号
            return Ok((statements, None));
        }
        
        // 解析一个语句或表达式
        let expr = parser.expression()?;
        
        // 检查是否是块的最后一个表达式
        if parser.check(&TokenType::RightBrace) {
            parser.advance(); // 消费右花括号
            return Ok((statements, Some(Box::new(expr))));
        }
        
        // 不是最后一个表达式，将其作为语句添加
        statements.push(Stmt::Expression(expr));
    }
}

fn parse_block(parser: &mut Parser) -> Result<Expr, ParseError> {
    parser.consume(TokenType::LeftBrace, "Expect '{' before block")?;
    
    let mut statements = Vec::new();
    let mut final_expr = None;
    
    // 如果块为空或只有一个表达式
    if !parser.check(&TokenType::RightBrace) && !parser.is_at_end() {
        // 检查是否是语句（以 let, effect, type 开头）
        if parser.check(&TokenType::Let) || parser.check(&TokenType::Effect) || parser.check(&TokenType::Type) {
            // 解析语句序列
            while !parser.check(&TokenType::RightBrace) && !parser.is_at_end() {
                statements.push(parser.statement()?);
            }
        } else {
            // 只有一个表达式
            final_expr = Some(Box::new(parse_expression(parser)?));
        }
    }
    
    parser.consume(TokenType::RightBrace, "Expect '}' after block")?;
    
    Ok(Expr::Block(statements, final_expr))
}

fn parse_perform(parser: &mut Parser) -> Result<Expr, ParseError> {
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

fn parse_handle(parser: &mut Parser) -> Result<Expr, ParseError> {
    // 解析被处理的表达式
    let expr = parse_expression(parser)?;
    
    parser.consume(TokenType::LeftBrace, "Expect '{' after expression in handle")?;
    
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

fn parse_match(parser: &mut Parser) -> Result<Expr, ParseError> {
    // 解析被匹配的表达式
    let expr = parse_expression(parser)?;
    
    parser.consume(TokenType::LeftBrace, "Expect '{' after expression in match")?;
    
    // 解析匹配分支
    let mut cases = Vec::new();
    
    while !parser.check(&TokenType::RightBrace) && !parser.is_at_end() {
        // 解析模式
        let pattern_name = if let TokenType::Identifier(name) = &parser.peek().token_type {
            name.clone()
        } else {
            return Err(ParseError::UnexpectedToken {
                expected: "pattern name".to_string(),
                found: format!("{:?}", parser.peek().token_type),
                line: parser.peek().line,
                column: parser.peek().column,
            });
        };
        parser.advance();
        
        // 解析模式参数
        let pattern_params = if parser.match_token(&[TokenType::LeftParen]) {
            let mut params = Vec::new();
            
            if !parser.check(&TokenType::RightParen) {
                loop {
                    // 递归解析嵌套模式
                    let sub_pattern_name = if let TokenType::Identifier(name) = &parser.peek().token_type {
                        name.clone()
                    } else {
                        return Err(ParseError::UnexpectedToken {
                            expected: "pattern parameter name".to_string(),
                            found: format!("{:?}", parser.peek().token_type),
                            line: parser.peek().line,
                            column: parser.peek().column,
                        });
                    };
                    parser.advance();
                    
                    // 简化处理，不支持嵌套模式参数
                    params.push(Pattern {
                        name: sub_pattern_name,
                        params: None,
                    });
                    
                    if !parser.match_token(&[TokenType::Comma]) {
                        break;
                    }
                }
            }
            
            parser.consume(TokenType::RightParen, "Expect ')' after pattern parameters")?;
            
            Some(params)
        } else {
            None
        };
        
        let pattern = Pattern {
            name: pattern_name,
            params: pattern_params,
        };
        
        parser.consume(TokenType::FatArrow, "Expect '=>' after pattern")?;
        
        // 解析分支体
        let body = parse_expression(parser)?;
        
        cases.push(MatchCase {
            pattern,
            body,
        });
        
        // 消费逗号（如果有）
        parser.match_token(&[TokenType::Comma]);
    }
    
    parser.consume(TokenType::RightBrace, "Expect '}' after match cases")?;
    
    Ok(Expr::Match(Box::new(expr), cases))
}

// 添加解析对象字面量的函数
fn parse_object_literal(parser: &mut Parser) -> Result<Expr, ParseError> {
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

// 添加解析索引访问的函数（在 parse_call 函数中调用）
fn parse_index_access(parser: &mut Parser, expr: Expr) -> Result<Expr, ParseError> {
    // 解析索引表达式
    let index = parse_expression(parser)?;
    
    // 消费右方括号
    parser.consume(TokenType::RightBracket, "Expect ']' after index")?;
    
    // 创建索引访问表达式（使用二元操作符实现）
    Ok(Expr::Binary(
        Box::new(expr),
        BinaryOp::Index,
        Box::new(index)
    ))
}


fn parse_match_expression(parser: &mut Parser) -> Result<Expr, ParseError> {
    let value = parse_expression(parser)?;
    
    parser.consume(TokenType::LeftBrace, "Expect '{' after match value")?;
    
    let mut cases = Vec::new();
    
    while !parser.check(&TokenType::RightBrace) && !parser.is_at_end() {
        let pattern = parse_pattern(parser)?;
        
        parser.consume(TokenType::Arrow, "Expect '=>' after pattern")?;
        
        let body = parse_expression(parser)?;
        
        cases.push(crate::ast::MatchCase { pattern, body });
        
        // 如果有逗号，消耗它
        parser.match_token(&[TokenType::Comma]);
    }
    
    parser.consume(TokenType::RightBrace, "Expect '}' after match cases")?;
    
    Ok(Expr::Match(Box::new(value), cases))
}

// 添加解析模式的函数
fn parse_pattern(parser: &mut Parser) -> Result<crate::ast::Pattern, ParseError> {
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
            
            Ok(crate::ast::Pattern {
                name,
                params: Some(params),
            })
        } else {
            // 没有参数的简单模式
            Ok(crate::ast::Pattern {
                name,
                params: None,
            })
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

// 添加对块表达式的解析支持
fn parse_block_expression(parser: &mut Parser) -> Result<Expr, ParseError> {
    parser.consume(TokenType::LeftBrace, "Expect '{' at the beginning of a block")?;
    
    let (statements, expr) = parse_block_contents(parser)?;
    
    Ok(Expr::Block(statements, expr))
}
