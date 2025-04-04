use thiserror::Error;

#[derive(Error, Debug)]
pub enum TypeError {
    #[error("类型不匹配: 期望 {expected}, 得到 {actual}")]
    TypeMismatch {
        expected: String,
        actual: String,
    },
    #[error("无法推导类型: {0}")]
    InferenceFailure(String),
    #[error("未定义的变量: {0}")]
    UndefinedVariable(String),
    #[error("递归类型定义")]
    RecursiveType,
    #[error("类型变量逃逸")]
    TypeVariableEscape,
    #[error("未知类型: {0}")]
    UnknownType(String),
    #[error("类型不统一: {0} 和 {1}")]
    UnificationFailure(String, String),
}

pub type TypeResult<T> = Result<T, TypeError>;