use crate::runtime::{Value, Environment, RuntimeError};
use std::rc::Rc;
use std::cell::RefCell;

pub fn register(env: &Rc<RefCell<Environment>>) {
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
}