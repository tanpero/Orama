use crate::ast::{FunctionType, TypeAnnotation};
use crate::parser::{ParseError, Parser};
use crate::token::TokenType;

pub fn parse_type_annotation(parser: &mut Parser) -> Result<TypeAnnotation, ParseError> {
    // 检查是否是效应类型
    if parser.match_token(&[TokenType::LessThan]) {
        let mut effects = Vec::new();
        
        if !parser.check(&TokenType::GreaterThan) {
            loop {
                if let TokenType::Identifier(effect) = &parser.peek().token_type {
                    effects.push(effect.clone());
                    parser.advance();
                } else {
                    return Err(ParseError::UnexpectedToken {
                        expected: "effect name".to_string(),
                        found: format!("{:?}", parser.peek().token_type),
                        line: parser.peek().line,
                        column: parser.peek().column,
                    });
                }
                
                if !parser.match_token(&[TokenType::Comma]) {
                    break;
                }
            }
        }
        
        parser.consume(TokenType::GreaterThan, "Expect '>' after effect list")?;
        
        let return_type = Box::new(parse_type_annotation(parser)?);
        
        return Ok(TypeAnnotation::Effect(effects, return_type));
    }
    
    // 检查是否是函数类型
    if parser.match_token(&[TokenType::LeftParen]) {
        let mut params = Vec::new();
        
        if !parser.check(&TokenType::RightParen) {
            loop {
                params.push(parse_type_annotation(parser)?);
                
                if !parser.match_token(&[TokenType::Comma]) {
                    break;
                }
            }
        }
        
        parser.consume(TokenType::RightParen, "Expect ')' after parameter types")?;
        parser.consume(TokenType::Arrow, "Expect '=>' after parameter types")?;
        
        let return_type = Box::new(parse_type_annotation(parser)?);
        
        return Ok(TypeAnnotation::Function(FunctionType {
            params,
            return_type,
        }));
    }
    
    // 简单类型
    if let TokenType::Identifier(name) = &parser.peek().token_type {
        let type_name = name.clone();
        parser.advance();
        
        // 检查是否有泛型参数
        let type_args = if parser.match_token(&[TokenType::LessThan]) {
            let mut args = Vec::new();
            
            if !parser.check(&TokenType::GreaterThan) {
                loop {
                    args.push(parse_type_annotation(parser)?);
                    
                    if !parser.match_token(&[TokenType::Comma]) {
                        break;
                    }
                }
            }
            
            parser.consume(TokenType::GreaterThan, "Expect '>' after type arguments")?;
            
            args
        } else {
            Vec::new()
        };
        
        Ok(TypeAnnotation::Simple(type_name, type_args))
    } else {
        Err(ParseError::UnexpectedToken {
            expected: "type name".to_string(),
            found: format!("{:?}", parser.peek().token_type),
            line: parser.peek().line,
            column: parser.peek().column,
        })
    }
}

pub fn parse_function_type(parser: &mut Parser) -> Result<FunctionType, ParseError> {
    parser.consume(TokenType::LeftParen, "Expect '(' at start of function type")?;
    
    let mut params = Vec::new();
    
    if !parser.check(&TokenType::RightParen) {
        loop {
            params.push(parse_type_annotation(parser)?);
            
            if !parser.match_token(&[TokenType::Comma]) {
                break;
            }
        }
    }
    
    parser.consume(TokenType::RightParen, "Expect ')' after parameter types")?;
    parser.consume(TokenType::Arrow, "Expect '=>' after parameter types")?;
    
    let return_type = Box::new(parse_type_annotation(parser)?);
    
    Ok(FunctionType {
        params,
        return_type,
    })
}