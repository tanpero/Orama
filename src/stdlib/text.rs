use crate::runtime::{Value, Environment, RuntimeError};
use std::rc::Rc;
use std::cell::RefCell;

pub fn register(env: &Rc<RefCell<Environment>>) {
    super::add_native_fn(
        env,
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
    
    super::add_native_fn(
        env,
        "to_string",
        vec!["value"],
        |args, _| {
            if let Some(value) = args.get(0) {
                Ok(Value::String(super::format_value(value)))
            } else {
                Err(RuntimeError::ArgumentMismatch { expected: 1, actual: 0 })
            }
        },
    );
    
    super::add_native_fn(
        env,
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
}