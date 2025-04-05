use crate::runtime::{Environment, RuntimeResult, Value};
use std::cell::RefCell;
use std::rc::Rc;

use std::fmt;

// 定义子模块
mod array;
mod file;
mod math;
mod stdio;
mod text; // 添加文件模块

pub fn create_stdlib() -> Rc<RefCell<Environment>> {
    let env = Rc::new(RefCell::new(Environment::new()));

    // 注册各个模块的函数
    stdio::register(&env);
    math::register(&env);
    text::register(&env);
    array::register(&env);
    file::register(&env); // 注册文件模块

    env
}

// 添加原生函数
fn add_native_fn<F>(env: &Rc<RefCell<Environment>>, name: &str, params: Vec<&str>, func: F)
where
    F: Fn(Vec<Value>, Rc<RefCell<Environment>>) -> RuntimeResult<Value> + 'static,
{
    let native_fn = NativeFunction {
        params: params.iter().map(|s| s.to_string()).collect(),
        func: Rc::new(func),
    };

    env.borrow_mut()
        .define(name.to_string(), Value::NativeFunction(native_fn));
}

// 格式化值为字符串（带类型信息，用于REPL）
pub fn format_value(value: &Value) -> String {
    let type_name = match value {
        Value::Number(_) => "Number",
        Value::String(_) => "String",
        Value::Boolean(_) => "Boolean",
        Value::Array(_) => "Array",
        Value::Object(_) => "Object",
        Value::Function(_) => "Function",
        Value::NativeFunction(_) => "NativeFunction",
        Value::Effect(_) => "Effect",
        Value::Null => "Null",
        Value::Unit => "Unit",
    };
    match value {
        Value::Number(n) => format!("{}: {}", n, type_name),
        Value::String(s) => format!("{}: {}", s, type_name),
        Value::Boolean(b) => format!("{}: {}", b, type_name),
        Value::Array(items) => {
            let items_str: Vec<String> = items.iter().map(|i| format_value(i)).collect();

            // 确定数组元素类型
            let array_type = if items.is_empty() {
                ""
            } else {
                // 从第一个元素推断类型
                let element_type = match &items[0] {
                    Value::Number(_) => "Number",
                    Value::String(_) => "String",
                    Value::Boolean(_) => "Boolean",
                    Value::Array(_) => "Array",
                    Value::Object(_) => "Object",
                    Value::Function(_) => "Function",
                    Value::NativeFunction(_) => "NativeFunction",
                    Value::Effect(_) => "Effect",
                    Value::Null => "Null",
                    Value::Unit => "Unit",
                };
                element_type
            };

            format!("[{}]: [{}]", items_str.join(", "), array_type)
        }
        Value::Object(fields) => {
            let fields_str: Vec<String> = fields
                .iter()
                .map(|(k, v)| format!("{}: {}", k, format_value(v)))
                .collect();
            format!("{{{}}}: Object", fields_str.join(", "))
        }
        Value::Function(f) => {
            // 为函数生成更详细的类型签名
            let params = f.params.join(", ");
            format!("({}) => <...>: Function", params)
        }
        Value::NativeFunction(f) => {
            let params = f.params.join(", ");
            format!("({}) => <native>: NativeFunction", params)
        }
        Value::Effect(_) => "<effect>: Effect".to_string(),
        Value::Null => "null: Null".to_string(),
        Value::Unit => "(): Unit".to_string(),
    }
}

// 格式化值为字符串（仅显示值，用于标准库函数）
pub fn format_value_simple(value: &Value) -> String {
    match value {
        Value::Number(n) => n.to_string(),
        Value::String(s) => s.clone(),
        Value::Boolean(b) => b.to_string(),
        Value::Array(items) => {
            let items_str: Vec<String> = items.iter().map(|i| format_value_simple(i)).collect();
            format!("[{}]", items_str.join(", "))
        },
        Value::Object(fields) => {
            let fields_str: Vec<String> = fields
                .iter()
                .map(|(k, v)| format!("{}: {}", k, format_value_simple(v)))
                .collect();
            format!("{{{}}}", fields_str.join(", "))
        },
        Value::Function(_) => "<function>".to_string(),
        Value::NativeFunction(_) => "<native function>".to_string(),
        Value::Effect(_) => "<effect>".to_string(),
        Value::Null => "null".to_string(),
        Value::Unit => "()".to_string(),
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
