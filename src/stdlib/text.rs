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

    // 子字符串提取函数
    super::add_native_fn(
        env,
        "substring",
        vec!["string", "start", "end"],
        |args, _| {
            if args.len() < 2 {
                return Err(RuntimeError::ArgumentMismatch { expected: 2, actual: args.len() });
            }
            
            match &args[0] {
                Value::String(s) => {
                    let start = match &args[1] {
                        Value::Number(n) => *n as usize,
                        _ => return Err(RuntimeError::TypeError("start 参数必须是数字".to_string())),
                    };
                    
                    let end = if args.len() > 2 {
                        match &args[2] {
                            Value::Number(n) => *n as usize,
                            _ => return Err(RuntimeError::TypeError("end 参数必须是数字".to_string())),
                        }
                    } else {
                        s.len()
                    };
                    
                    if start > s.len() {
                        return Ok(Value::String("".to_string()));
                    }
                    
                    let end = end.min(s.len());
                    
                    // 确保我们在字符边界上切割
                    let s_chars: Vec<char> = s.chars().collect();
                    let start = start.min(s_chars.len());
                    let end = end.min(s_chars.len());
                    
                    let result: String = s_chars[start..end].iter().collect();
                    Ok(Value::String(result))
                },
                _ => Err(RuntimeError::TypeError("substring 函数需要一个字符串参数".to_string())),
            }
        },
    );
    
    // 字符串分割函数
    super::add_native_fn(
        env,
        "split",
        vec!["string", "delimiter"],
        |args, _| {
            if args.len() < 2 {
                return Err(RuntimeError::ArgumentMismatch { expected: 2, actual: args.len() });
            }
            
            match (&args[0], &args[1]) {
                (Value::String(s), Value::String(delim)) => {
                    let parts: Vec<Value> = s.split(delim)
                        .map(|part| Value::String(part.to_string()))
                        .collect();
                    Ok(Value::Array(parts))
                },
                _ => Err(RuntimeError::TypeError("split 函数需要两个字符串参数".to_string())),
            }
        },
    );
    
    // 字符串连接函数
    super::add_native_fn(
        env,
        "join",
        vec!["array", "delimiter"],
        |args, _| {
            if args.len() < 2 {
                return Err(RuntimeError::ArgumentMismatch { expected: 2, actual: args.len() });
            }
            
            match (&args[0], &args[1]) {
                (Value::Array(arr), Value::String(delim)) => {
                    let strings: Result<Vec<String>, RuntimeError> = arr.iter()
                        .map(|val| match val {
                            Value::String(s) => Ok(s.clone()),
                            _ => Ok(super::format_value(val)),
                        })
                        .collect();
                    
                    match strings {
                        Ok(strs) => Ok(Value::String(strs.join(delim))),
                        Err(e) => Err(e),
                    }
                },
                _ => Err(RuntimeError::TypeError("join 函数需要一个数组和一个字符串参数".to_string())),
            }
        },
    );
    
    // 字符串替换函数
    super::add_native_fn(
        env,
        "replace",
        vec!["string", "pattern", "replacement"],
        |args, _| {
            if args.len() < 3 {
                return Err(RuntimeError::ArgumentMismatch { expected: 3, actual: args.len() });
            }
            
            match (&args[0], &args[1], &args[2]) {
                (Value::String(s), Value::String(pattern), Value::String(replacement)) => {
                    Ok(Value::String(s.replace(pattern, replacement)))
                },
                _ => Err(RuntimeError::TypeError("replace 函数需要三个字符串参数".to_string())),
            }
        },
    );
    
    // 字符串转大写函数
    super::add_native_fn(
        env,
        "to_upper",
        vec!["string"],
        |args, _| {
            if args.is_empty() {
                return Err(RuntimeError::ArgumentMismatch { expected: 1, actual: 0 });
            }
            
            match &args[0] {
                Value::String(s) => Ok(Value::String(s.to_uppercase())),
                _ => Err(RuntimeError::TypeError("to_upper 函数需要一个字符串参数".to_string())),
            }
        },
    );
    
    // 字符串转小写函数
    super::add_native_fn(
        env,
        "to_lower",
        vec!["string"],
        |args, _| {
            if args.is_empty() {
                return Err(RuntimeError::ArgumentMismatch { expected: 1, actual: 0 });
            }
            
            match &args[0] {
                Value::String(s) => Ok(Value::String(s.to_lowercase())),
                _ => Err(RuntimeError::TypeError("to_lower 函数需要一个字符串参数".to_string())),
            }
        },
    );
    
    // 字符串修剪函数（去除首尾空白）
    super::add_native_fn(
        env,
        "trim",
        vec!["string"],
        |args, _| {
            if args.is_empty() {
                return Err(RuntimeError::ArgumentMismatch { expected: 1, actual: 0 });
            }
            
            match &args[0] {
                Value::String(s) => Ok(Value::String(s.trim().to_string())),
                _ => Err(RuntimeError::TypeError("trim 函数需要一个字符串参数".to_string())),
            }
        },
    );
    
    // 字符串包含检查函数
    super::add_native_fn(
        env,
        "contains",
        vec!["string", "substring"],
        |args, _| {
            if args.len() < 2 {
                return Err(RuntimeError::ArgumentMismatch { expected: 2, actual: args.len() });
            }
            
            match (&args[0], &args[1]) {
                (Value::String(s), Value::String(substr)) => {
                    Ok(Value::Boolean(s.contains(substr)))
                },
                _ => Err(RuntimeError::TypeError("contains 函数需要两个字符串参数".to_string())),
            }
        },
    );
    
    // 字符串开头检查函数
    super::add_native_fn(
        env,
        "starts_with",
        vec!["string", "prefix"],
        |args, _| {
            if args.len() < 2 {
                return Err(RuntimeError::ArgumentMismatch { expected: 2, actual: args.len() });
            }
            
            match (&args[0], &args[1]) {
                (Value::String(s), Value::String(prefix)) => {
                    Ok(Value::Boolean(s.starts_with(prefix)))
                },
                _ => Err(RuntimeError::TypeError("starts_with 函数需要两个字符串参数".to_string())),
            }
        },
    );
    
    // 字符串结尾检查函数
    super::add_native_fn(
        env,
        "ends_with",
        vec!["string", "suffix"],
        |args, _| {
            if args.len() < 2 {
                return Err(RuntimeError::ArgumentMismatch { expected: 2, actual: args.len() });
            }
            
            match (&args[0], &args[1]) {
                (Value::String(s), Value::String(suffix)) => {
                    Ok(Value::Boolean(s.ends_with(suffix)))
                },
                _ => Err(RuntimeError::TypeError("ends_with 函数需要两个字符串参数".to_string())),
            }
        },
    );
}