use std::collections::HashMap;
use crate::runtime::{Value, Function, Environment, RuntimeResult, RuntimeError};
use std::rc::Rc;
use std::cell::RefCell;
use crate::ast::Expr;

use std::fmt;

pub fn create_stdlib() -> Rc<RefCell<Environment>> {
    let env = Rc::new(RefCell::new(Environment::new()));
    
    // 添加打印函数
    add_native_fn(
        &env,
        "print",
        vec!["value"],
        |args, _| {
            if let Some(value) = args.get(0) {
                println!("{}", format_value(value));
                Ok(Value::Null)
            } else {
                println!();
                Ok(Value::Null)
            }
        },
    );
    
    // 添加数学函数
    add_native_fn(
        &env,
        "sqrt",
        vec!["x"],
        |args, _| {
            if let Some(Value::Number(x)) = args.get(0) {
                Ok(Value::Number(x.sqrt()))
            } else {
                Err(RuntimeError::TypeError("sqrt 函数需要一个数字参数".to_string()))
            }
        },
    );
    
    add_native_fn(
        &env,
        "sin",
        vec!["x"],
        |args, _| {
            if let Some(Value::Number(x)) = args.get(0) {
                Ok(Value::Number(x.sin()))
            } else {
                Err(RuntimeError::TypeError("sin 函数需要一个数字参数".to_string()))
            }
        },
    );
    
    add_native_fn(
        &env,
        "cos",
        vec!["x"],
        |args, _| {
            if let Some(Value::Number(x)) = args.get(0) {
                Ok(Value::Number(x.cos()))
            } else {
                Err(RuntimeError::TypeError("cos 函数需要一个数字参数".to_string()))
            }
        },
    );
    
    // 添加字符串函数
    add_native_fn(
        &env,
        "length",
        vec!["value"],
        |args, _| {
            if let Some(value) = args.get(0) {
                match value {
                    Value::String(s) => Ok(Value::Number(s.len() as f64)),
                    Value::Array(a) => Ok(Value::Number(a.len() as f64)),
                    _ => Err(RuntimeError::TypeError("length 函数需要一个字符串或数组参数".to_string())),
                }
            } else {
                Err(RuntimeError::ArgumentMismatch { expected: 1, actual: 0 })
            }
        },
    );
    
    // 添加数组函数
    add_native_fn(
        &env,
        "push",
        vec!["array", "value"],
        |args, _| {
            if args.len() < 2 {
                return Err(RuntimeError::ArgumentMismatch { expected: 2, actual: args.len() });
            }
            
            if let Value::Array(mut array) = args[0].clone() {
                array.push(args[1].clone());
                Ok(Value::Array(array))
            } else {
                Err(RuntimeError::TypeError("push 函数的第一个参数必须是数组".to_string()))
            }
        },
    );
    
    // 添加类型转换函数
    add_native_fn(
        &env,
        "to_string",
        vec!["value"],
        |args, _| {
            if let Some(value) = args.get(0) {
                Ok(Value::String(format_value(value)))
            } else {
                Err(RuntimeError::ArgumentMismatch { expected: 1, actual: 0 })
            }
        },
    );
    
    add_native_fn(
        &env,
        "to_number",
        vec!["value"],
        |args, _| {
            if let Some(value) = args.get(0) {
                match value {
                    Value::String(s) => {
                        match s.parse::<f64>() {
                            Ok(n) => Ok(Value::Number(n)),
                            Err(_) => Err(RuntimeError::TypeError(format!("无法将字符串 '{}' 转换为数字", s))),
                        }
                    },
                    Value::Number(n) => Ok(Value::Number(*n)),
                    Value::Boolean(b) => Ok(Value::Number(if *b { 1.0 } else { 0.0 })),
                    _ => Err(RuntimeError::TypeError("无法转换为数字".to_string())),
                }
            } else {
                Err(RuntimeError::ArgumentMismatch { expected: 1, actual: 0 })
            }
        },
    );
    
    env
}

// 添加原生函数
fn add_native_fn<F>(
    env: &Rc<RefCell<Environment>>,
    name: &str,
    params: Vec<&str>,
    func: F,
) where
    F: Fn(Vec<Value>, Rc<RefCell<Environment>>) -> RuntimeResult<Value> + 'static,
{
    let native_fn = NativeFunction {
        params: params.iter().map(|s| s.to_string()).collect(),
        func: Rc::new(func),
    };
    
    env.borrow_mut().define(
        name.to_string(),
        Value::NativeFunction(native_fn),
    );
}

// 格式化值为字符串
pub fn format_value(value: &Value) -> String {
    match value {
        Value::Number(n) => n.to_string(),
        Value::String(s) => s.clone(),
        Value::Boolean(b) => b.to_string(),
        Value::Array(items) => {
            let items_str: Vec<String> = items.iter().map(|i| format_value(i)).collect();
            format!("[{}]", items_str.join(", "))
        },
        Value::Object(fields) => {
            let fields_str: Vec<String> = fields
                .iter()
                .map(|(k, v)| format!("{}: {}", k, format_value(v)))
                .collect();
            format!("{{{}}}", fields_str.join(", "))
        },
        Value::Function(_) => "<function>".to_string(),
        Value::NativeFunction(_) => "<native function>".to_string(),
        Value::Effect(_) => "<effect>".to_string(),
        Value::Null => "null".to_string(),
    }
}

// Original struct definition
#[derive(Clone)]
pub struct NativeFunction {
    pub params: Vec<String>,
    pub func: Rc<dyn Fn(Vec<Value>, Rc<RefCell<Environment>>) -> RuntimeResult<Value>>,
}

// Add a custom Debug implementation
impl fmt::Debug for NativeFunction {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("NativeFunction")
            .field("params", &self.params)
            .field("func", &"<function>")
            .finish()
    }
}