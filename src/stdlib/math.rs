use crate::runtime::{Environment, RuntimeError, Value};
use std::cell::RefCell;
use std::f64::consts;
use std::rc::Rc;

pub fn register(env: &Rc<RefCell<Environment>>) {
    // 数学常量
    env.borrow_mut()
        .define("PI".to_string(), Value::Number(consts::PI));
    env.borrow_mut()
        .define("E".to_string(), Value::Number(consts::E));

    // 平方根
    super::add_native_fn(env, "sqrt", vec!["x"], |args, _| {
        if let Some(Value::Number(x)) = args.get(0) {
            Ok(Value::Number(x.sqrt()))
        } else {
            Err(RuntimeError::TypeError(
                "sqrt 函数需要一个数字参数".to_string(),
            ))
        }
    });

    // 三角函数
    super::add_native_fn(env, "sin", vec!["x"], |args, _| {
        if let Some(Value::Number(x)) = args.get(0) {
            Ok(Value::Number(x.sin()))
        } else {
            Err(RuntimeError::TypeError(
                "sin 函数需要一个数字参数".to_string(),
            ))
        }
    });

    super::add_native_fn(env, "cos", vec!["x"], |args, _| {
        if let Some(Value::Number(x)) = args.get(0) {
            Ok(Value::Number(x.cos()))
        } else {
            Err(RuntimeError::TypeError(
                "cos 函数需要一个数字参数".to_string(),
            ))
        }
    });

    // 添加更多三角函数
    super::add_native_fn(env, "tan", vec!["x"], |args, _| {
        if let Some(Value::Number(x)) = args.get(0) {
            Ok(Value::Number(x.tan()))
        } else {
            Err(RuntimeError::TypeError(
                "tan 函数需要一个数字参数".to_string(),
            ))
        }
    });

    super::add_native_fn(env, "asin", vec!["x"], |args, _| {
        if let Some(Value::Number(x)) = args.get(0) {
            if *x >= -1.0 && *x <= 1.0 {
                Ok(Value::Number(x.asin()))
            } else {
                Err(RuntimeError::TypeError(
                    "asin 函数的参数必须在 -1 到 1 之间".to_string(),
                ))
            }
        } else {
            Err(RuntimeError::TypeError(
                "asin 函数需要一个数字参数".to_string(),
            ))
        }
    });

    super::add_native_fn(env, "acos", vec!["x"], |args, _| {
        if let Some(Value::Number(x)) = args.get(0) {
            if *x >= -1.0 && *x <= 1.0 {
                Ok(Value::Number(x.acos()))
            } else {
                Err(RuntimeError::TypeError(
                    "acos 函数的参数必须在 -1 到 1 之间".to_string(),
                ))
            }
        } else {
            Err(RuntimeError::TypeError(
                "acos 函数需要一个数字参数".to_string(),
            ))
        }
    });

    super::add_native_fn(env, "atan", vec!["x"], |args, _| {
        if let Some(Value::Number(x)) = args.get(0) {
            Ok(Value::Number(x.atan()))
        } else {
            Err(RuntimeError::TypeError(
                "atan 函数需要一个数字参数".to_string(),
            ))
        }
    });

    super::add_native_fn(env, "atan2", vec!["y", "x"], |args, _| {
        if args.len() < 2 {
            return Err(RuntimeError::ArgumentMismatch {
                expected: 2,
                actual: args.len(),
            });
        }

        if let (Value::Number(y), Value::Number(x)) = (&args[0], &args[1]) {
            Ok(Value::Number(y.atan2(*x)))
        } else {
            Err(RuntimeError::TypeError(
                "atan2 函数需要两个数字参数".to_string(),
            ))
        }
    });

    // 双曲函数
    super::add_native_fn(env, "sinh", vec!["x"], |args, _| {
        if let Some(Value::Number(x)) = args.get(0) {
            Ok(Value::Number(x.sinh()))
        } else {
            Err(RuntimeError::TypeError(
                "sinh 函数需要一个数字参数".to_string(),
            ))
        }
    });

    super::add_native_fn(env, "cosh", vec!["x"], |args, _| {
        if let Some(Value::Number(x)) = args.get(0) {
            Ok(Value::Number(x.cosh()))
        } else {
            Err(RuntimeError::TypeError(
                "cosh 函数需要一个数字参数".to_string(),
            ))
        }
    });

    super::add_native_fn(env, "tanh", vec!["x"], |args, _| {
        if let Some(Value::Number(x)) = args.get(0) {
            Ok(Value::Number(x.tanh()))
        } else {
            Err(RuntimeError::TypeError(
                "tanh 函数需要一个数字参数".to_string(),
            ))
        }
    });

    // 指数和对数函数
    super::add_native_fn(env, "exp", vec!["x"], |args, _| {
        if let Some(Value::Number(x)) = args.get(0) {
            Ok(Value::Number(x.exp()))
        } else {
            Err(RuntimeError::TypeError(
                "exp 函数需要一个数字参数".to_string(),
            ))
        }
    });

    super::add_native_fn(env, "ln", vec!["x"], |args, _| {
        if let Some(Value::Number(x)) = args.get(0) {
            if *x > 0.0 {
                Ok(Value::Number(x.ln()))
            } else {
                Err(RuntimeError::TypeError(
                    "ln 函数的参数必须大于 0".to_string(),
                ))
            }
        } else {
            Err(RuntimeError::TypeError(
                "ln 函数需要一个数字参数".to_string(),
            ))
        }
    });

    super::add_native_fn(env, "log10", vec!["x"], |args, _| {
        if let Some(Value::Number(x)) = args.get(0) {
            if *x > 0.0 {
                Ok(Value::Number(x.log10()))
            } else {
                Err(RuntimeError::TypeError(
                    "log10 函数的参数必须大于 0".to_string(),
                ))
            }
        } else {
            Err(RuntimeError::TypeError(
                "log10 函数需要一个数字参数".to_string(),
            ))
        }
    });

    super::add_native_fn(env, "log", vec!["x", "base"], |args, _| {
        if args.len() < 2 {
            return Err(RuntimeError::ArgumentMismatch {
                expected: 2,
                actual: args.len(),
            });
        }

        if let (Value::Number(x), Value::Number(base)) = (&args[0], &args[1]) {
            if *x <= 0.0 || *base <= 0.0 || *base == 1.0 {
                Err(RuntimeError::TypeError(
                    "log 函数的参数和底数必须大于 0，且底数不能为 1".to_string(),
                ))
            } else {
                Ok(Value::Number(x.log(*base)))
            }
        } else {
            Err(RuntimeError::TypeError(
                "log 函数需要两个数字参数".to_string(),
            ))
        }
    });

    super::add_native_fn(env, "pow", vec!["x", "y"], |args, _| {
        if args.len() < 2 {
            return Err(RuntimeError::ArgumentMismatch {
                expected: 2,
                actual: args.len(),
            });
        }

        if let (Value::Number(x), Value::Number(y)) = (&args[0], &args[1]) {
            Ok(Value::Number(x.powf(*y)))
        } else {
            Err(RuntimeError::TypeError(
                "pow 函数需要两个数字参数".to_string(),
            ))
        }
    });

    // 取整函数
    super::add_native_fn(env, "floor", vec!["x"], |args, _| {
        if let Some(Value::Number(x)) = args.get(0) {
            Ok(Value::Number(x.floor()))
        } else {
            Err(RuntimeError::TypeError(
                "floor 函数需要一个数字参数".to_string(),
            ))
        }
    });

    super::add_native_fn(env, "ceil", vec!["x"], |args, _| {
        if let Some(Value::Number(x)) = args.get(0) {
            Ok(Value::Number(x.ceil()))
        } else {
            Err(RuntimeError::TypeError(
                "ceil 函数需要一个数字参数".to_string(),
            ))
        }
    });

    super::add_native_fn(env, "round", vec!["x"], |args, _| {
        if let Some(Value::Number(x)) = args.get(0) {
            Ok(Value::Number(x.round()))
        } else {
            Err(RuntimeError::TypeError(
                "round 函数需要一个数字参数".to_string(),
            ))
        }
    });

    super::add_native_fn(env, "trunc", vec!["x"], |args, _| {
        if let Some(Value::Number(x)) = args.get(0) {
            Ok(Value::Number(x.trunc()))
        } else {
            Err(RuntimeError::TypeError(
                "trunc 函数需要一个数字参数".to_string(),
            ))
        }
    });

    // 绝对值
    super::add_native_fn(env, "abs", vec!["x"], |args, _| {
        if let Some(Value::Number(x)) = args.get(0) {
            Ok(Value::Number(x.abs()))
        } else {
            Err(RuntimeError::TypeError(
                "abs 函数需要一个数字参数".to_string(),
            ))
        }
    });

    // 最大值和最小值
    super::add_native_fn(env, "max", vec!["x", "y"], |args, _| {
        if args.len() < 2 {
            return Err(RuntimeError::ArgumentMismatch {
                expected: 2,
                actual: args.len(),
            });
        }

        if let (Value::Number(x), Value::Number(y)) = (&args[0], &args[1]) {
            Ok(Value::Number(x.max(*y)))
        } else {
            Err(RuntimeError::TypeError(
                "max 函数需要两个数字参数".to_string(),
            ))
        }
    });

    super::add_native_fn(env, "min", vec!["x", "y"], |args, _| {
        if args.len() < 2 {
            return Err(RuntimeError::ArgumentMismatch {
                expected: 2,
                actual: args.len(),
            });
        }

        if let (Value::Number(x), Value::Number(y)) = (&args[0], &args[1]) {
            Ok(Value::Number(x.min(*y)))
        } else {
            Err(RuntimeError::TypeError(
                "min 函数需要两个数字参数".to_string(),
            ))
        }
    });

    // 随机数
    super::add_native_fn(env, "random", vec![], |_, _| {
        use rand::Rng;
        let mut rng = rand::rng();
        Ok(Value::Number(rng.random::<f64>()))
    });

    super::add_native_fn(env, "random_range", vec!["min", "max"], |args, _| {
        if args.len() < 2 {
            return Err(RuntimeError::ArgumentMismatch {
                expected: 2,
                actual: args.len(),
            });
        }

        if let (Value::Number(min), Value::Number(max)) = (&args[0], &args[1]) {
            if min >= max {
                return Err(RuntimeError::TypeError(
                    "random_range 函数的最小值必须小于最大值".to_string(),
                ));
            }

            use rand::Rng;
            let mut rng = rand::rng();
            Ok(Value::Number(rng.random_range(*min..*max)))
        } else {
            Err(RuntimeError::TypeError(
                "random_range 函数需要两个数字参数".to_string(),
            ))
        }
    });

    // 判断函数
    super::add_native_fn(env, "is_nan", vec!["x"], |args, _| {
        if let Some(Value::Number(x)) = args.get(0) {
            Ok(Value::Boolean(x.is_nan()))
        } else {
            Err(RuntimeError::TypeError(
                "is_nan 函数需要一个数字参数".to_string(),
            ))
        }
    });

    super::add_native_fn(env, "is_finite", vec!["x"], |args, _| {
        if let Some(Value::Number(x)) = args.get(0) {
            Ok(Value::Boolean(x.is_finite()))
        } else {
            Err(RuntimeError::TypeError(
                "is_finite 函数需要一个数字参数".to_string(),
            ))
        }
    });

    super::add_native_fn(env, "is_infinite", vec!["x"], |args, _| {
        if let Some(Value::Number(x)) = args.get(0) {
            Ok(Value::Boolean(x.is_infinite()))
        } else {
            Err(RuntimeError::TypeError(
                "is_infinite 函数需要一个数字参数".to_string(),
            ))
        }
    });
}
