use crate::ast::{EffectSignature, FunctionType, Stmt, TypeAnnotation, TypeDefinition, TypeField, TypeVariant};
use crate::parser::{ParseError, Parser};
use crate::token::TokenType;
use crate::parser::type_parser;
use crate::ast::Parameter;

pub fn parse_variable_declaration(parser: &mut Parser) -> Result<Stmt, ParseError> {
    let name = if let TokenType::Identifier(name) = &parser.peek().token_type {
        name.clone()
    } else {
        return Err(ParseError::UnexpectedToken {
            expected: "identifier".to_string(),
            found: format!("{:?}", parser.peek().token_type),
            line: parser.peek().line,
            column: parser.peek().column,
        });
    };
    parser.advance();
    
    parser.consume(TokenType::Equal, "Expect '=' after variable name")?;
    
    let initializer = parser.expression()?;
    
    Ok(Stmt::VariableDecl(name, initializer))
}

pub fn parse_effect_declaration(parser: &mut Parser) -> Result<Stmt, ParseError> {
    let name = if let TokenType::Identifier(name) = &parser.peek().token_type {
        name.clone()
    } else {
        return Err(ParseError::UnexpectedToken {
            expected: "identifier".to_string(),
            found: format!("{:?}", parser.peek().token_type),
            line: parser.peek().line,
            column: parser.peek().column,
        });
    };
    parser.advance();
    
    // 解析可选的类型参数
    let type_params = if parser.match_token(&[TokenType::LessThan]) {
        let mut params = Vec::new();
        
        loop {
            if let TokenType::Identifier(param) = &parser.peek().token_type {
                params.push(param.clone());
                parser.advance();
            } else {
                return Err(ParseError::UnexpectedToken {
                    expected: "type parameter".to_string(),
                    found: format!("{:?}", parser.peek().token_type),
                    line: parser.peek().line,
                    column: parser.peek().column,
                });
            }
            
            if !parser.match_token(&[TokenType::Comma]) {
                break;
            }
        }
        
        parser.consume(TokenType::GreaterThan, "Expect '>' after type parameters")?;
        
        Some(params)
    } else {
        None
    };
    
    parser.consume(TokenType::LeftBrace, "Expect '{' after effect name")?;
    
    // 解析效应操作列表
    let mut operations = Vec::new();
    
    while !parser.check(&TokenType::RightBrace) && !parser.is_at_end() {
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
        
        // 简化处理，实际应该解析函数类型
        let function_type = FunctionType {
            params: Vec::new(),
            return_type: Box::new(TypeAnnotation::Simple("Any".to_string(), Some(Vec::new()))),
        };
        
        operations.push(EffectSignature {
            name: op_name,
            function_type,
        });
        
        if !parser.match_token(&[TokenType::Comma]) {
            break;
        }
    }
    
    parser.consume(TokenType::RightBrace, "Expect '}' after effect operations")?;
    
    Ok(Stmt::EffectDecl(name, type_params, operations))
}

pub fn parse_type_declaration(parser: &mut Parser) -> Result<Stmt, ParseError> {
    let name = if let TokenType::Identifier(name) = &parser.peek().token_type {
        name.clone()
    } else {
        return Err(ParseError::UnexpectedToken {
            expected: "identifier".to_string(),
            found: format!("{:?}", parser.peek().token_type),
            line: parser.peek().line,
            column: parser.peek().column,
        });
    };
    parser.advance();
    
    // 解析可选的类型参数
    let type_params = if parser.match_token(&[TokenType::LessThan]) {
        let mut params = Vec::new();
        
        loop {
            if let TokenType::Identifier(param) = &parser.peek().token_type {
                params.push(param.clone());
                parser.advance();
            } else {
                return Err(ParseError::UnexpectedToken {
                    expected: "type parameter".to_string(),
                    found: format!("{:?}", parser.peek().token_type),
                    line: parser.peek().line,
                    column: parser.peek().column,
                });
            }
            
            if !parser.match_token(&[TokenType::Comma]) {
                break;
            }
        }
        
        parser.consume(TokenType::GreaterThan, "Expect '>' after type parameters")?;
        
        Some(params)
    } else {
        None
    };
    
    parser.consume(TokenType::Equal, "Expect '=' after type name")?;
    
    // 解析类型定义
    let type_def = if parser.match_token(&[TokenType::Pipe]) {
        // 联合类型
        let mut variants = Vec::new();
        
        loop {
            let variant_name = if let TokenType::Identifier(name) = &parser.peek().token_type {
                name.clone()
            } else {
                return Err(ParseError::UnexpectedToken {
                    expected: "variant name".to_string(),
                    found: format!("{:?}", parser.peek().token_type),
                    line: parser.peek().line,
                    column: parser.peek().column,
                });
            };
            parser.advance();
            
            // 解析可选的参数列表
            let params = if parser.match_token(&[TokenType::LeftParen]) {
                let params = parse_parameter_list(parser)?;
                parser.consume(TokenType::RightParen, "Expect ')' after variant parameters")?;
                Some(params)
            } else {
                None
            };
            
            variants.push(TypeVariant {
                name: variant_name,
                params,
            });
            
            if !parser.match_token(&[TokenType::Pipe]) {
                break;
            }
        }
        
        TypeDefinition::Union(variants)
    } else if parser.match_token(&[TokenType::LeftBrace]) {
        // 记录类型
        let mut fields = Vec::new();
        
        while !parser.check(&TokenType::RightBrace) && !parser.is_at_end() {
            let field_name = if let TokenType::Identifier(name) = &parser.peek().token_type {
                name.clone()
            } else {
                return Err(ParseError::UnexpectedToken {
                    expected: "field name".to_string(),
                    found: format!("{:?}", parser.peek().token_type),
                    line: parser.peek().line,
                    column: parser.peek().column,
                });
            };
            parser.advance();
            
            parser.consume(TokenType::Colon, "Expect ':' after field name")?;
            
            let type_annotation = type_parser::parse_type_annotation(parser)?;
            
            fields.push(TypeField {
                name: field_name,
                type_annotation,
            });
            
            if !parser.match_token(&[TokenType::Comma]) && !parser.check(&TokenType::RightBrace) {
                return Err(ParseError::UnexpectedToken {
                    expected: "comma or '}'".to_string(),
                    found: format!("{:?}", parser.peek().token_type),
                    line: parser.peek().line,
                    column: parser.peek().column,
                });
            }
        }
        
        parser.consume(TokenType::RightBrace, "Expect '}' after record fields")?;
        
        TypeDefinition::Record(fields)
    } else {
        return Err(ParseError::UnexpectedToken {
            expected: "'|' or '{'".to_string(),
            found: format!("{:?}", parser.peek().token_type),
            line: parser.peek().line,
            column: parser.peek().column,
        });
    };
    
    Ok(Stmt::TypeDecl(name, type_params, type_def))
}

fn parse_parameter_list(parser: &mut Parser) -> Result<Vec<Parameter>, ParseError> {
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
            
            // 解析可选的类型注解
            let type_annotation = if parser.match_token(&[TokenType::Colon]) {
                Some(parse_type_annotation(parser)?)
            } else {
                None
            };
            
            params.push(Parameter {
                name: param_name,
                type_annotation,
            });
            
            if !parser.match_token(&[TokenType::Comma]) {
                break;
            }
        }
    }
    
    Ok(params)
}

fn parse_type_annotation(parser: &mut Parser) -> Result<TypeAnnotation, ParseError> {
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
        
        Ok(TypeAnnotation::Simple(type_name, Some(type_args)))
    } else {
        Err(ParseError::UnexpectedToken {
            expected: "type name".to_string(),
            found: format!("{:?}", parser.peek().token_type),
            line: parser.peek().line,
            column: parser.peek().column,
        })
    }
}