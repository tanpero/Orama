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

    // map 函数：对数组中的每个元素应用一个函数，返回新数组
    super::add_native_fn(
        env,
        "map",
        vec!["array", "callback"],
        |args, env| {
            if args.len() < 2 {
                return Err(RuntimeError::ArgumentMismatch { expected: 2, actual: args.len() });
            }
            
            if let Value::Array(array) = &args[0] {
                match &args[1] {
                    Value::Function(_) | Value::NativeFunction(_) => {
                        let mut result = Vec::with_capacity(array.len());
                        
                        for item in array {
                            // 直接调用函数，而不是通过 evaluator
                            match &args[1] {
                                Value::Function(func) => {
                                    // 创建一个临时环境
                                    let mut temp_env = Environment::with_enclosing(Rc::clone(&func.closure));
                                    
                                    // 绑定参数
                                    if func.params.len() > 0 {
                                        temp_env.define(func.params[0].clone(), item.clone());
                                    }
                                    
                                    // 创建一个临时的 evaluator 来执行函数
                                    let mut temp_evaluator = crate::evaluator::Evaluator::with_environment(
                                        Rc::new(RefCell::new(temp_env))
                                    );
                                    
                                    // 执行函数体
                                    let mapped_value = temp_evaluator.evaluate_expression(&func.body)?;
                                    result.push(mapped_value);
                                },
                                Value::NativeFunction(native_fn) => {
                                    let mapped_value = (native_fn.func)(vec![item.clone()], Rc::clone(&env))?;
                                    result.push(mapped_value);
                                },
                                _ => unreachable!(),
                            }
                        }
                        
                        Ok(Value::Array(result))
                    },
                    _ => Err(RuntimeError::TypeError("map 函数的第二个参数必须是函数".to_string()))
                }
            } else {
                Err(RuntimeError::TypeError("map 函数的第一个参数必须是数组".to_string()))
            }
        },
    );

    // filter 函数：返回数组中满足条件的元素
    super::add_native_fn(
        env,
        "filter",
        vec!["array", "callback"],
        |args, env| {
            if args.len() < 2 {
                return Err(RuntimeError::ArgumentMismatch { expected: 2, actual: args.len() });
            }
            
            if let Value::Array(array) = &args[0] {
                match &args[1] {
                    Value::Function(_) | Value::NativeFunction(_) => {
                        let mut result = Vec::new();
                        
                        for item in array {
                            // 直接调用函数，而不是通过 evaluator
                            let condition = match &args[1] {
                                Value::Function(func) => {
                                    // 创建一个临时环境
                                    let mut temp_env = Environment::with_enclosing(Rc::clone(&func.closure));
                                    
                                    // 绑定参数
                                    if func.params.len() > 0 {
                                        temp_env.define(func.params[0].clone(), item.clone());
                                    }
                                    
                                    // 创建一个临时的 evaluator 来执行函数
                                    let mut temp_evaluator = crate::evaluator::Evaluator::with_environment(
                                        Rc::new(RefCell::new(temp_env))
                                    );
                                    
                                    // 执行函数体
                                    temp_evaluator.evaluate_expression(&func.body)?
                                },
                                Value::NativeFunction(native_fn) => {
                                    (native_fn.func)(vec![item.clone()], Rc::clone(&env))?
                                },
                                _ => unreachable!(),
                            };
                            
                            if let Value::Boolean(true) = condition {
                                result.push(item.clone());
                            }
                        }
                        
                        Ok(Value::Array(result))
                    },
                    _ => Err(RuntimeError::TypeError("filter 函数的第二个参数必须是函数".to_string()))
                }
            } else {
                Err(RuntimeError::TypeError("filter 函数的第一个参数必须是数组".to_string()))
            }
        },
    );

    // reduce 函数：将数组归约为单个值
    super::add_native_fn(
        env,
        "reduce",
        vec!["array", "callback", "initial"],
        |args, env| {
            if args.len() < 3 {
                return Err(RuntimeError::ArgumentMismatch { expected: 3, actual: args.len() });
            }
            
            if let Value::Array(array) = &args[0] {
                match &args[1] {
                    Value::Function(_) | Value::NativeFunction(_) => {
                        let mut accumulator = args[2].clone();
                        
                        for item in array {
                            // 直接调用函数，而不是通过 evaluator
                            accumulator = match &args[1] {
                                Value::Function(func) => {
                                    // 创建一个临时环境
                                    let mut temp_env = Environment::with_enclosing(Rc::clone(&func.closure));
                                    
                                    // 绑定参数
                                    if func.params.len() > 0 {
                                        temp_env.define(func.params[0].clone(), accumulator);
                                    }
                                    if func.params.len() > 1 {
                                        temp_env.define(func.params[1].clone(), item.clone());
                                    }
                                    
                                    // 创建一个临时的 evaluator 来执行函数
                                    let mut temp_evaluator = crate::evaluator::Evaluator::with_environment(
                                        Rc::new(RefCell::new(temp_env))
                                    );
                                    
                                    // 执行函数体
                                    temp_evaluator.evaluate_expression(&func.body)?
                                },
                                Value::NativeFunction(native_fn) => {
                                    (native_fn.func)(vec![accumulator, item.clone()], Rc::clone(&env))?
                                },
                                _ => unreachable!(),
                            };
                        }
                        
                        Ok(accumulator)
                    },
                    _ => Err(RuntimeError::TypeError("reduce 函数的第二个参数必须是函数".to_string()))
                }
            } else {
                Err(RuntimeError::TypeError("reduce 函数的第一个参数必须是数组".to_string()))
            }
        },
    );

    // each 函数：对数组中的每个元素执行一个函数，不返回新数组
    super::add_native_fn(
        env,
        "each",
        vec!["array", "callback"],
        |args, env| {
            if args.len() < 2 {
                return Err(RuntimeError::ArgumentMismatch { expected: 2, actual: args.len() });
            }
            
            if let Value::Array(array) = &args[0] {
                match &args[1] {
                    Value::Function(_) | Value::NativeFunction(_) => {
                        for item in array {
                            // 直接调用函数，而不是通过 evaluator
                            match &args[1] {
                                Value::Function(func) => {
                                    // 创建一个临时环境
                                    let mut temp_env = Environment::with_enclosing(Rc::clone(&func.closure));
                                    
                                    // 绑定参数
                                    if func.params.len() > 0 {
                                        temp_env.define(func.params[0].clone(), item.clone());
                                    }
                                    
                                    // 创建一个临时的 evaluator 来执行函数
                                    let mut temp_evaluator = crate::evaluator::Evaluator::with_environment(
                                        Rc::new(RefCell::new(temp_env))
                                    );
                                    
                                    // 执行函数体，忽略返回值
                                    temp_evaluator.evaluate_expression(&func.body)?;
                                },
                                Value::NativeFunction(native_fn) => {
                                    // 调用原生函数，忽略返回值
                                    (native_fn.func)(vec![item.clone()], Rc::clone(&env))?;
                                },
                                _ => unreachable!(),
                            }
                        }
                        
                        // 返回原数组
                        Ok(args[0].clone())
                    },
                    _ => Err(RuntimeError::TypeError("each 函数的第二个参数必须是函数".to_string()))
                }
            } else {
                Err(RuntimeError::TypeError("each 函数的第一个参数必须是数组".to_string()))
            }
        },
    );
}