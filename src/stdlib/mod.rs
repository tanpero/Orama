use crate::runtime::{Value, Environment, RuntimeResult};
use std::rc::Rc;
use std::cell::RefCell;

use std::fmt;

// 定义子模块
mod stdio;
mod math;
mod text;
mod array;

pub fn create_stdlib() -> Rc<RefCell<Environment>> {
    let env = Rc::new(RefCell::new(Environment::new()));
    
    // 注册各个模块的函数
    stdio::register(&env);
    math::register(&env);
    text::register(&env);
    array::register(&env);
    
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
        &Value::Unit => "()".to_string(),
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