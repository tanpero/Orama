use crate::ast::{Expr, Literal, TypeAnnotation, BinaryOp, UnaryOp};
use thiserror::Error;

#[derive(Error, Debug)]
pub enum TypeError {
    #[error("类型不匹配: 期望 {expected}, 得到 {actual}")]
    TypeMismatch {
        expected: String,
        actual: String,
    },
    #[error("数组元素类型不一致: 位置 {index} 的元素类型为 {element_type}, 与数组类型 {array_type} 不匹配")]
    ArrayElementTypeMismatch {
        index: usize,
        element_type: String,
        array_type: String,
    },
    #[error("未知类型: {0}")]
    UnknownType(String),
}

pub type TypeResult<T> = Result<T, TypeError>;

#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    Number,
    String,
    Boolean,
    Array(Box<Type>),
    Any,
    Null,
}

impl std::fmt::Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Type::Number => write!(f, "Number"),
            Type::String => write!(f, "String"),
            Type::Boolean => write!(f, "Boolean"),
            Type::Array(element_type) => write!(f, "[{}]", element_type),
            Type::Any => write!(f, "Any"),
            Type::Null => write!(f, "Null"),
        }
    }
}

pub fn check_expr(expr: &Expr) -> TypeResult<Type> {
    match expr {
        Expr::Literal(lit) => check_literal(lit),
        // 其他表达式类型的检查可以在这里添加
        _ => Ok(Type::Any), // 暂时对其他表达式返回Any类型
    }
}

pub fn check_literal(lit: &Literal) -> TypeResult<Type> {
    match lit {
        Literal::Number(_) => Ok(Type::Number),
        Literal::String(_) => Ok(Type::String),
        Literal::Boolean(_) => Ok(Type::Boolean),
        Literal::Null => Ok(Type::Null),
        Literal::Array(elements) => {
            if elements.is_empty() {
                // 空数组，无法确定类型，返回Any
                return Ok(Type::Array(Box::new(Type::Any)));
            }
            
            // 检查第一个元素的类型
            let first_type = check_expr(&elements[0])?;
            
            // 检查所有其他元素是否与第一个元素类型相同
            for (i, element) in elements.iter().enumerate().skip(1) {
                let element_type = check_expr(element)?;
                if element_type != first_type {
                    return Err(TypeError::ArrayElementTypeMismatch {
                        index: i,
                        element_type: element_type.to_string(),
                        array_type: first_type.to_string(),
                    });
                }
            }
            
            Ok(Type::Array(Box::new(first_type)))
        },
        Literal::Object(_) => Ok(Type::Any), // 暂时对对象返回Any类型
    }
}