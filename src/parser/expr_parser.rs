use crate::ast::{BinaryOp, Expr, Literal, Parameter, UnaryOp};
use crate::parser::{ParseError, Parser};
use crate::token::TokenType;

// Fixing imports to use the correct paths
use crate::parser::control_flow_parser;
use crate::parser::function_parser;
use crate::parser::function_parser::*;
use crate::parser::literal_parser;

pub fn parse_expression(parser: &mut Parser) -> Result<Expr, ParseError> {
    // 检查是否是函数表达式
    if parser.check(&TokenType::LeftParen)
        && parser.check_next(&TokenType::RightParen)
        && (parser.check_ahead(2, &TokenType::FatArrow)
            || parser.check_ahead(2, &TokenType::LeftBrace))
    {
        return parse_empty_param_function(parser);
    }

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

    while parser.match_token(&[TokenType::Or, TokenType::LogicalOr]) {
        let right = parse_and(parser)?;
        expr = Expr::Binary(Box::new(expr), BinaryOp::Or, Box::new(right));
    }

    Ok(expr)
}

fn parse_and(parser: &mut Parser) -> Result<Expr, ParseError> {
    let mut expr = parse_equality(parser)?;

    while parser.match_token(&[TokenType::And, TokenType::LogicalAnd]) {
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
        TokenType::LessThan,    // 添加 LessThan 作为 Less 的替代
        TokenType::GreaterThan, // 添加 GreaterThan 作为 Greater 的替代
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
            expr = literal_parser::parse_index_access(parser, expr)?;
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

    parser.consume(TokenType::RightParen, "Expect ')' after arguments")?;

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
    if parser.match_token(&[TokenType::LeftBrace]) {
        return literal_parser::parse_object_literal(parser);
    }
    // 添加对 if 表达式的支持
    if parser.match_token(&[TokenType::If]) {
        return control_flow_parser::parse_if_expression(parser);
    }

    if parser.match_token(&[TokenType::Match]) {
        return control_flow_parser::parse_match_expression(parser);
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
        return literal_parser::parse_array_literal(parser);
    }

    if parser.match_token(&[TokenType::LeftParen]) {
        return function_parser::parse_parenthesized_expr_or_function(parser);
    }

    // 添加一个 else 子句，处理所有其他情况
    Err(ParseError::UnexpectedToken {
        expected: "expression".to_string(),
        found: format!("{:?}", parser.peek().token_type),
        line: parser.peek().line,
        column: parser.peek().column,
    })
}

// Make sure this function is public and exported
pub fn parse_block_contents(
    parser: &mut Parser,
) -> Result<(Vec<crate::parser::Stmt>, Option<Box<Expr>>), ParseError> {
    let mut statements = Vec::new();
    let mut last_expr = None;

    while !parser.check(&TokenType::RightBrace) && !parser.is_at_end() {
        // 尝试解析语句
        if parser.check(&TokenType::Let)
            || parser.check(&TokenType::Effect)
            || parser.check(&TokenType::Type)
        {
            statements.push(parser.statement()?);
        } else {
            // 如果不是语句开始标记，则解析表达式
            let expr = parse_expression(parser)?;

            // 检查是否是块中的最后一个表达式
            if parser.check(&TokenType::RightBrace) {
                last_expr = Some(Box::new(expr));
            } else {
                // 否则将其作为表达式语句添加
                statements.push(crate::parser::Stmt::Expression(expr));
            }
        }
    }

    parser.consume(TokenType::RightBrace, "Expect '}' after block")?;

    Ok((statements, last_expr))
}
