use crate::ast::{BinaryOp, Expr, Literal, Parameter, UnaryOp};
use crate::parser::{ParseError, Parser};
use crate::token::TokenType;

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
    ]) {
        let operator = match parser.previous().token_type {
            TokenType::Less => BinaryOp::Less,
            TokenType::Greater => BinaryOp::Greater,
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
        } else if parser.match_token(&[TokenType::Dot]) {
            let name = parser.consume(TokenType::Identifier("".to_string()), "Expect property name after '.'")?;
            if let TokenType::Identifier(name) = &name.token_type {
                expr = Expr::Variable(name.clone()); // 简化处理，实际应该是属性访问
            } else {
                unreachable!();
            }
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

    if parser.match_token(&[TokenType::LeftParen]) {
        if parser.match_token(&[TokenType::RightParen]) {
            // 空参数列表的函数
            if parser.match_token(&[TokenType::FatArrow]) {
                let body = parse_expression(parser)?;
                return Ok(Expr::Function(Vec::new(), Box::new(body)));
            }
        } else {
            // 带参数的函数或分组表达式
            let expr = parse_expression(parser)?;
            parser.consume(TokenType::RightParen, "Expect ')' after expression")?;
            
            if parser.match_token(&[TokenType::FatArrow]) {
                // 这是一个函数定义
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

    if parser.match_token(&[TokenType::If]) {
        return parse_if_expression(parser);
    }

    if parser.match_token(&[TokenType::LeftBrace]) {
        return parse_block(parser);
    }

    if parser.match_token(&[TokenType::Perform]) {
        return parse_perform(parser);
    }

    if parser.match_token(&[TokenType::Handle]) {
        return parse_handle(parser);
    }

    if parser.match_token(&[TokenType::Match]) {
        return parse_match(parser);
    }

    Err(ParseError::UnexpectedToken {
        expected: "expression".to_string(),
        found: format!("{:?}", parser.peek().token_type),
        line: parser.peek().line,
        column: parser.peek().column,
    })
}

fn parse_if_expression(parser: &mut Parser) -> Result<Expr, ParseError> {
    let condition = parse_expression(parser)?;
    
    let then_branch = parse_block(parser)?;
    
    let else_branch = if parser.match_token(&[TokenType::Else]) {
        if parser.check(&TokenType::If) {
            Some(Box::new(parse_if_expression(parser)?))
        } else {
            Some(Box::new(parse_block(parser)?))
        }
    } else {
        None
    };
    
    Ok(Expr::If(Box::new(condition), Box::new(then_branch), else_branch))
}

fn parse_block(parser: &mut Parser) -> Result<Expr, ParseError> {
    let mut statements = Vec::new();
    let mut final_expr = None;
    
    while !parser.check(&TokenType::RightBrace) && !parser.is_at_end() {
        if parser.check(&TokenType::RightBrace) {
            break;
        }
        
        // 如果不是语句结尾，可能是最后的表达式
        if statements.len() > 0 && !parser.check(&TokenType::Let) && 
           !parser.check(&TokenType::Effect) && !parser.check(&TokenType::Type) {
            final_expr = Some(Box::new(parse_expression(parser)?));
            break;
        }
        
        statements.push(parser.statement()?);
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
    // 这里简化了处理，实际应该更复杂
    let expr = parse_expression(parser)?;
    
    parser.consume(TokenType::LeftBrace, "Expect '{' after expression in handle")?;
    
    // 简化处理，实际应该解析效应处理器
    parser.consume(TokenType::RightBrace, "Expect '}' after handle body")?;
    
    Ok(Expr::Handle(Box::new(expr), Vec::new(), None))
}

fn parse_match(parser: &mut Parser) -> Result<Expr, ParseError> {
    // 这里简化了处理，实际应该更复杂
    let expr = parse_expression(parser)?;
    
    parser.consume(TokenType::LeftBrace, "Expect '{' after expression in match")?;
    
    // 简化处理，实际应该解析匹配分支
    parser.consume(TokenType::RightBrace, "Expect '}' after match body")?;
    
    Ok(Expr::Match(Box::new(expr), Vec::new()))
}