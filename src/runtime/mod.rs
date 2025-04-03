use std::collections::HashMap;
use std::rc::Rc;
use std::cell::RefCell;
use crate::ast::{Expr, Stmt, Literal, BinaryOp, UnaryOp};

// 运行时值类型
// 在 Value 枚举中添加 NativeFunction 变体
#[derive(Clone, Debug)]
pub enum Value {
    Number(f64),
    String(String),
    Boolean(bool),
    Array(Vec<Value>),
    Object(HashMap<String, Value>),
    Function(Function),
    NativeFunction(crate::stdlib::NativeFunction),
    Effect(Effect),
    Null,
}

// 函数类型
#[derive(Clone, Debug)]
pub struct Function {
    pub params: Vec<String>,
    pub body: Rc<Expr>,
    pub closure: Rc<RefCell<Environment>>,
}

// 效应类型
#[derive(Clone, Debug)]
pub struct Effect {
    pub name: String,
    pub operations: HashMap<String, Function>,
}

// 环境类型，用于存储变量
#[derive(Debug)]
pub struct Environment {
    values: HashMap<String, Value>,
    enclosing: Option<Rc<RefCell<Environment>>>,
}

impl Environment {
    pub fn new() -> Self {
        Environment {
            values: HashMap::new(),
            enclosing: None,
        }
    }
    
    pub fn with_enclosing(enclosing: Rc<RefCell<Environment>>) -> Self {
        Environment {
            values: HashMap::new(),
            enclosing: Some(enclosing),
        }
    }
    
    pub fn define(&mut self, name: String, value: Value) {
        self.values.insert(name, value);
    }
    
    pub fn get(&self, name: &str) -> Option<Value> {
        if let Some(value) = self.values.get(name) {
            Some(value.clone())
        } else if let Some(enclosing) = &self.enclosing {
            enclosing.borrow().get(name)
        } else {
            None
        }
    }
    
    pub fn assign(&mut self, name: &str, value: Value) -> bool {
        if self.values.contains_key(name) {
            self.values.insert(name.to_string(), value);
            true
        } else if let Some(enclosing) = &self.enclosing {
            enclosing.borrow_mut().assign(name, value)
        } else {
            false
        }
    }
    
    // Add this getter method for enclosing environment
    pub fn get_enclosing(&self) -> Option<Rc<RefCell<Environment>>> {
        self.enclosing.clone()
    }
}

// 运行时错误类型
#[derive(Debug, thiserror::Error)]
pub enum RuntimeError {
    #[error("未定义的变量: {0}")]
    UndefinedVariable(String),
    
    #[error("类型错误: {0}")]
    TypeError(String),
    
    #[error("未处理的效应: {0}.{1}")]
    UnhandledEffect(String, String),
    
    #[error("参数数量不匹配: 期望 {expected}, 实际 {actual}")]
    ArgumentMismatch { expected: usize, actual: usize },
    
    #[error("运行时错误: {0}")]
    Generic(String),
}

// 运行时结果类型
pub type RuntimeResult<T> = Result<T, RuntimeError>;