use super::format_value_simple;
use crate::runtime::{Environment, Value};
use std::cell::RefCell;
use std::io::{self, BufRead, Write};
use std::rc::Rc;

pub fn register(env: &Rc<RefCell<Environment>>) {
    super::add_native_fn(env, "print", vec!["value"], |args, _| {
        if let Some(value) = args.get(0) {
            print!("{}", format_value_simple(value));
            Ok(Value::Null)
        } else {
            print!("{}", "".to_string());
            Ok(Value::Null)
        }
    });

    // 添加 println 函数 - 与 print 类似但总是添加换行符
    super::add_native_fn(env, "println", vec!["value"], |args, _| {
        if let Some(value) = args.get(0) {
            println!("{}", format_value_simple(value));
        } else {
            println!();
        }
        Ok(Value::Null)
    });

    // 添加 input 函数 - 从标准输入读取一行文本
    super::add_native_fn(env, "input", vec!["prompt"], |args, _| {
        if let Some(prompt) = args.get(0) {
            print!("{}\n", format_value_simple(prompt));
            io::stdout().flush().unwrap();
        }

        let mut input = String::new();
        io::stdin().lock().read_line(&mut input).unwrap();

        // 移除末尾的换行符
        if input.ends_with('\n') {
            input.pop();
            if input.ends_with('\r') {
                input.pop();
            }
        }

        Ok(Value::String(input))
    });

    // 添加 printf 函数 - 格式化输出，不添加换行符
    super::add_native_fn(env, "printf", vec!["format", "args"], |args, _| {
        if let Some(format_str) = args.get(0) {
            if let Value::String(format_string) = format_str {
                let mut result = format_string.clone();

                if args.len() > 1 {
                    for (i, arg) in args.iter().skip(1).enumerate() {
                        let placeholder = format!("{{{}}}", i);
                        result = result.replace(&placeholder, &format_value_simple(arg));
                    }
                }

                print!("{}", result);
                io::stdout().flush().unwrap();
            } else {
                print!("{}", format_value_simple(format_str));
                io::stdout().flush().unwrap();
            }
        }

        Ok(Value::Null)
    });
}
