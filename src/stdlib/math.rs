use crate::runtime::{Value, Environment, RuntimeResult, RuntimeError};
use std::rc::Rc;
use std::cell::RefCell;

pub fn register(env: &Rc<RefCell<Environment>>) {
    super::add_native_fn(
        env,
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
    
    super::add_native_fn(
        env,
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
    
    super::add_native_fn(
        env,
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
}