use crate::runtime::{Value, Environment, RuntimeResult};
use std::rc::Rc;
use std::cell::RefCell;
use super::format_value;

pub fn register(env: &Rc<RefCell<Environment>>) {
    super::add_native_fn(
        env,
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
}