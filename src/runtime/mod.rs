use std::collections::HashMap;
use std::rc::Rc;
use std::cell::RefCell;
use crate::ast::{Expr};

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
    Unit,
}

// 函数类型
// 在 Function 结构体中添加类型信息字段
#[derive(Clone, Debug)]
pub struct Function {
    pub params: Vec<String>,
    pub param_types: Vec<Option<String>>, // 参数类型（如果有）
    pub return_type: Option<String>,      // 返回类型（如果有）
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

    #[error("索引超出范围: 索引 {index} 超出数组大小 {size}")]
    IndexOutOfBounds { index: usize, size: usize },

    #[error("无效的操作: {0}")]
    IOError(String),
    
    #[error("运行时错误: {0}")]
    Generic(String),
}

// 运行时结果类型
pub type RuntimeResult<T> = Result<T, RuntimeError>;

// 处理 Value 类型的比较问题

// 在 Value 枚举中添加自定义比较方法
impl Value {
    pub fn is_equal(&self, other: &Value) -> bool {
        match (self, other) {
            (Value::Number(a), Value::Number(b)) => a == b,
            (Value::String(a), Value::String(b)) => a == b,
            (Value::Boolean(a), Value::Boolean(b)) => a == b,
            (Value::Null, Value::Null) => true,
            (Value::Unit, Value::Unit) => true,
            (Value::Array(a), Value::Array(b)) => {
                if a.len() != b.len() {
                    return false;
                }
                for (i, item) in a.iter().enumerate() {
                    if !item.is_equal(&b[i]) {
                        return false;
                    }
                }
                true
            },
            (Value::Object(a), Value::Object(b)) => {
                if a.len() != b.len() {
                    return false;
                }
                for (key, val) in a {
                    match b.get(key) {
                        Some(other_val) => {
                            if !val.is_equal(other_val) {
                                return false;
                            }
                        },
                        None => return false,
                    }
                }
                true
            },
            // 函数、原生函数和效应不进行比较，总是返回 false
            _ => false,
        }
    }
}
