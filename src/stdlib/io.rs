use std::fs;
use std::io::{self, Read, Write};
use crate::runtime::{Value, RuntimeResult, RuntimeError, Effect};
use std::collections::HashMap;

pub fn create_io_effect() -> Effect {
    let mut operations = HashMap::new();
    
    // 读取文件操作
    operations.insert(
        "readFile".to_string(),
        crate::runtime::Function {
            params: vec!["path".to_string()],
            body: Rc::new(crate::ast::Expr::Literal(crate::ast::Literal::Null)),
            closure: Rc::new(RefCell::new(crate::runtime::Environment::new())),
            native_impl: Some(Rc::new(|args, _| {
                if args.len() != 1 {
                    return Err(RuntimeError::ArgumentMismatch {
                        expected: 1,
                        actual: args.len(),
                    });
                }
                
                if let Value::String(path) = &args[0] {
                    match fs::read_to_string(path) {
                        Ok(content) => Ok(Value::String(content)),
                        Err(e) => Err(RuntimeError::Generic(format!("读取文件错误: {}", e))),
                    }
                } else {
                    Err(RuntimeError::TypeError("readFile 需要一个字符串路径参数".to_string()))
                }
            })),
        },
    );
    
    // 写入文件操作
    operations.insert(
        "writeFile".to_string(),
        crate::runtime::Function {
            params: vec!["path".to_string(), "content".to_string()],
            body: Rc::new(crate::ast::Expr::Literal(crate::ast::Literal::Null)),
            closure: Rc::new(RefCell::new(crate::runtime::Environment::new())),
            native_impl: Some(Rc::new(|args, _| {
                if args.len() != 2 {
                    return Err(RuntimeError::ArgumentMismatch {
                        expected: 2,
                        actual: args.len(),
                    });
                }
                
                if let (Value::String(path), Value::String(content)) = (&args[0], &args[1]) {
                    match fs::write(path, content) {
                        Ok(_) => Ok(Value::Null),
                        Err(e) => Err(RuntimeError::Generic(format!("写入文件错误: {}", e))),
                    }
                } else {
                    Err(RuntimeError::TypeError("writeFile 需要字符串路径和内容参数".to_string()))
                }
            })),
        },
    );
    
    Effect {
        name: "IO".to_string(),
        operations,
    }
}