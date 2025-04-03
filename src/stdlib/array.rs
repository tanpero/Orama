use crate::runtime::{Value, Environment, RuntimeError};
use std::rc::Rc;
use std::cell::RefCell;

pub fn register(env: &Rc<RefCell<Environment>>) {
    // 已有的 push 函数
    super::add_native_fn(
        env,
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

    // 获取数组长度
    super::add_native_fn(
        env,
        "len",
        vec!["array"],
        |args, _| {
            if args.len() < 1 {
                return Err(RuntimeError::ArgumentMismatch { expected: 1, actual: args.len() });
            }
            
            if let Value::Array(array) = &args[0] {
                Ok(Value::Number(array.len() as f64))
            } else {
                Err(RuntimeError::TypeError("len 函数的参数必须是数组".to_string()))
            }
        },
    );

    // 获取数组元素
    super::add_native_fn(
        env,
        "get",
        vec!["array", "index"],
        |args, _| {
            if args.len() < 2 {
                return Err(RuntimeError::ArgumentMismatch { expected: 2, actual: args.len() });
            }
            
            if let Value::Array(array) = &args[0] {
                if let Value::Number(index) = args[1] {
                    let idx = index as usize;
                    if idx < array.len() {
                        Ok(array[idx].clone())
                    } else {
                        Err(RuntimeError::IndexOutOfBounds { index: idx, size: array.len() })
                    }
                } else {
                    Err(RuntimeError::TypeError("get 函数的第二个参数必须是数字".to_string()))
                }
            } else {
                Err(RuntimeError::TypeError("get 函数的第一个参数必须是数组".to_string()))
            }
        },
    );

    // 设置数组元素
    super::add_native_fn(
        env,
        "set",
        vec!["array", "index", "value"],
        |args, _| {
            if args.len() < 3 {
                return Err(RuntimeError::ArgumentMismatch { expected: 3, actual: args.len() });
            }
            
            if let Value::Array(mut array) = args[0].clone() {
                if let Value::Number(index) = args[1] {
                    let idx = index as usize;
                    if idx < array.len() {
                        array[idx] = args[2].clone();
                        Ok(Value::Array(array))
                    } else {
                        Err(RuntimeError::IndexOutOfBounds { index: idx, size: array.len() })
                    }
                } else {
                    Err(RuntimeError::TypeError("set 函数的第二个参数必须是数字".to_string()))
                }
            } else {
                Err(RuntimeError::TypeError("set 函数的第一个参数必须是数组".to_string()))
            }
        },
    );

    // 删除并返回数组最后一个元素
    super::add_native_fn(
        env,
        "pop",
        vec!["array"],
        |args, _| {
            if args.len() < 1 {
                return Err(RuntimeError::ArgumentMismatch { expected: 1, actual: args.len() });
            }
            
            if let Value::Array(mut array) = args[0].clone() {
                if array.is_empty() {
                    Err(RuntimeError::TypeError("无法从空数组中弹出元素".to_string()))
                } else {
                    Ok(array.pop().unwrap())
                }
            } else {
                Err(RuntimeError::TypeError("pop 函数的参数必须是数组".to_string()))
            }
        },
    );

    // 连接两个数组
    super::add_native_fn(
        env,
        "concat",
        vec!["array1", "array2"],
        |args, _| {
            if args.len() < 2 {
                return Err(RuntimeError::ArgumentMismatch { expected: 2, actual: args.len() });
            }
            
            if let Value::Array(mut array1) = args[0].clone() {
                if let Value::Array(array2) = &args[1] {
                    for item in array2 {
                        array1.push(item.clone());
                    }
                    Ok(Value::Array(array1))
                } else {
                    Err(RuntimeError::TypeError("concat 函数的第二个参数必须是数组".to_string()))
                }
            } else {
                Err(RuntimeError::TypeError("concat 函数的第一个参数必须是数组".to_string()))
            }
        },
    );

    // 创建一个新数组
    super::add_native_fn(
        env,
        "new",
        vec![],
        |_, _| {
            Ok(Value::Array(Vec::new()))
        },
    );

    // 切片数组
    super::add_native_fn(
        env,
        "slice",
        vec!["array", "start", "end"],
        |args, _| {
            if args.len() < 3 {
                return Err(RuntimeError::ArgumentMismatch { expected: 3, actual: args.len() });
            }
            
            if let Value::Array(array) = &args[0] {
                if let Value::Number(start) = args[1] {
                    if let Value::Number(end) = args[2] {
                        let start_idx = start as usize;
                        let end_idx = end as usize;
                        
                        if start_idx > end_idx || end_idx > array.len() {
                            return Err(RuntimeError::TypeError(format!(
                                "无效的切片范围: start={}, end={}, len={}", 
                                start_idx, end_idx, array.len()
                            )));
                        }
                        
                        let slice: Vec<Value> = array[start_idx..end_idx]
                            .iter()
                            .map(|v| v.clone())
                            .collect();
                            
                        Ok(Value::Array(slice))
                    } else {
                        Err(RuntimeError::TypeError("slice 函数的第三个参数必须是数字".to_string()))
                    }
                } else {
                    Err(RuntimeError::TypeError("slice 函数的第二个参数必须是数字".to_string()))
                }
            } else {
                Err(RuntimeError::TypeError("slice 函数的第一个参数必须是数组".to_string()))
            }
        },
    );

    // 判断数组是否包含某个元素
    super::add_native_fn(
        env,
        "contains",
        vec!["array", "value"],
        |args, _| {
            if args.len() < 2 {
                return Err(RuntimeError::ArgumentMismatch { expected: 2, actual: args.len() });
            }
            
            if let Value::Array(array) = &args[0] {
                for item in array {
                    if item.is_equal(&args[1]) {
                        return Ok(Value::Boolean(true));
                    }
                }
                Ok(Value::Boolean(false))
            } else {
                Err(RuntimeError::TypeError("contains 函数的第一个参数必须是数组".to_string()))
            }
        },
    );
}