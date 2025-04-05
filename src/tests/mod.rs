use crate::evaluator::Evaluator;
use crate::lexer;
use crate::parser;
use crate::runtime::Value;
use crate::stdlib;
use std::fs;
use std::path::Path;

// 测试辅助函数：执行代码并返回结果
fn execute_code(code: &str) -> Result<Value, String> {
    match lexer::lex(code) {
        Ok(tokens) => match parser::parse(tokens) {
            Ok(program) => {
                let stdlib_env = stdlib::create_stdlib();
                let mut evaluator = Evaluator::with_environment(stdlib_env);

                match evaluator.evaluate(&program) {
                    Ok(value) => Ok(value),
                    Err(e) => Err(format!("运行时错误: {}", e)),
                }
            }
            Err(e) => Err(format!("语法错误: {}", e)),
        },
        Err(e) => Err(format!("词法错误: {}", e)),
    }
}

// 测试辅助函数：从文件执行代码
fn execute_file(filename: &str) -> Result<Value, String> {
    match fs::read_to_string(filename) {
        Ok(source) => execute_code(&source),
        Err(e) => Err(format!("无法读取文件 '{}': {}", filename, e)),
    }
}

// 测试辅助函数：比较值是否相等
fn assert_value_eq(actual: &Value, expected: &Value) -> bool {
    match (actual, expected) {
        (Value::Number(a), Value::Number(b)) => (a - b).abs() < 1e-10,
        (Value::String(a), Value::String(b)) => a == b,
        (Value::Boolean(a), Value::Boolean(b)) => a == b,
        (Value::Array(a), Value::Array(b)) => {
            if a.len() != b.len() {
                return false;
            }
            for (a_item, b_item) in a.iter().zip(b.iter()) {
                if !assert_value_eq(a_item, b_item) {
                    return false;
                }
            }
            true
        }
        (Value::Object(a), Value::Object(b)) => {
            if a.len() != b.len() {
                return false;
            }
            for (key, a_val) in a {
                if let Some(b_val) = b.get(key) {
                    if !assert_value_eq(a_val, b_val) {
                        return false;
                    }
                } else {
                    return false;
                }
            }
            true
        }
        (Value::Null, Value::Null) => true,
        _ => false,
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::runtime::Value;

    #[test]
    fn test_basic_syntax() {
        // 变量声明和基本表达式
        let code = r#"
            let x = 5
            let y = 10
            let z = x + y
            z
        "#;

        let result = execute_code(code).unwrap();
        assert!(assert_value_eq(&result, &Value::Number(15.0)));

        // 字符串操作
        let code = r#"
            let greeting = "Hello"
            let name = "Orama"
            let message = greeting + " " + name
            message
        "#;

        let result = execute_code(code).unwrap();
        assert!(assert_value_eq(
            &result,
            &Value::String("Hello Orama".to_string())
        ));

        // 布尔值和条件表达式
        let code = r#"
            let a = true
            let b = false
            let c = a && b
            let d = a || b
            let result = if d { "yes" } else { "no" }
            result
        "#;

        let result = execute_code(code).unwrap();
        assert!(assert_value_eq(&result, &Value::String("yes".to_string())));
    }

    #[test]
    fn test_functions() {
        // 简单函数
        let code = r#"
            let add = (a, b) => a + b
            add(3, 4)
        "#;

        let result = execute_code(code).unwrap();
        assert!(assert_value_eq(&result, &Value::Number(7.0)));

        // 递归函数
        let code = r#"
            let factorial = (n) => 
                if n <= 1 {
                    1
                } else {
                    n * factorial(n - 1)
                }
            factorial(5)
        "#;

        let result = execute_code(code).unwrap();
        assert!(assert_value_eq(&result, &Value::Number(120.0)));

        // 闭包和高阶函数
        let code = r#"
            let makeAdder = (x) => (y) => x + y
            let add5 = makeAdder(5)
            add5(10)
        "#;

        let result = execute_code(code).unwrap();
        assert!(assert_value_eq(&result, &Value::Number(15.0)));
    }

    #[test]
    fn test_blocks_and_scopes() {
        // 块作用域
        let code = r#"
            let x = 10
            let result = {
                let x = 20
                let y = 30
                x + y
            }
            [x, result]
        "#;

        let result = execute_code(code).unwrap();
        if let Value::Array(values) = result {
            assert!(assert_value_eq(&values[0], &Value::Number(10.0)));
            assert!(assert_value_eq(&values[1], &Value::Number(50.0)));
        } else {
            panic!("Expected array result");
        }
    }

    #[test]
    fn test_data_structures() {
        // 数组
        let code = r#"
            let arr = [1, 2, 3, 4, 5]
            let sum = arr[0] + arr[1] + arr[2] + arr[3] + arr[4]
            sum
        "#;

        let result = execute_code(code).unwrap();
        assert!(assert_value_eq(&result, &Value::Number(15.0)));

        // 对象
        let code = r#"
            let person = {
                name: "Alice",
                age: 30,
                greet: (msg) => msg + " " + person.name
            }
            person.greet("Hello")
        "#;

        let result = execute_code(code).unwrap();
        assert!(assert_value_eq(
            &result,
            &Value::String("Hello Alice".to_string())
        ));
    }

    #[test]
    fn test_higher_order_functions() {
        // map 函数
        let code = r#"
            let map = (arr, fn) => {
                let result = []
                let i = 0
                while i < arr.length {
                    result.push(fn(arr[i]))
                    i = i + 1
                }
                result
            }
            
            let numbers = [1, 2, 3, 4, 5]
            let doubled = map(numbers, (x) => x * 2)
            doubled
        "#;

        let result = execute_code(code).unwrap();
        if let Value::Array(values) = result {
            assert_eq!(values.len(), 5);
            assert!(assert_value_eq(&values[0], &Value::Number(2.0)));
            assert!(assert_value_eq(&values[4], &Value::Number(10.0)));
        } else {
            panic!("Expected array result");
        }

        // filter 函数
        let code = r#"
            let filter = (arr, predicate) => {
                let result = []
                let i = 0
                while i < arr.length {
                    if predicate(arr[i]) {
                        result.push(arr[i])
                    }
                    i = i + 1
                }
                result
            }
            
            let numbers = [1, 2, 3, 4, 5]
            let evens = filter(numbers, (x) => x % 2 == 0)
            evens
        "#;

        let result = execute_code(code).unwrap();
        if let Value::Array(values) = result {
            assert_eq!(values.len(), 2);
            assert!(assert_value_eq(&values[0], &Value::Number(2.0)));
            assert!(assert_value_eq(&values[1], &Value::Number(4.0)));
        } else {
            panic!("Expected array result");
        }
    }

    #[test]
    fn test_pipe_operator() {
        // 管道操作符
        let code = r#"
            let double = (x) => x * 2
            let increment = (x) => x + 1
            let square = (x) => x * x
            
            let result = 5 |> double |> increment |> square
            result
        "#;

        let result = execute_code(code).unwrap();
        assert!(assert_value_eq(&result, &Value::Number(121.0)));
    }

    #[test]
    fn test_algebraic_effects() {
        // 简单的状态效应
        let code = r#"
            effect State {
                get: () => Number,
                set: (value: Number) => ()
            }
            
            let counter = () => {
                let current = perform State.get()
                perform State.set(current + 1)
                current
            }
            
            handle {
                let a = counter()
                let b = counter()
                let c = counter()
                [a, b, c]
            } {
                effect State {
                    get: () => {
                        let state = 0
                        resume(state)
                    },
                    set: (value) => {
                        resume((), value)
                    }
                }
            }
        "#;

        let result = execute_code(code).unwrap();
        if let Value::Array(values) = result {
            assert_eq!(values.len(), 3);
            assert!(assert_value_eq(&values[0], &Value::Number(0.0)));
            assert!(assert_value_eq(&values[1], &Value::Number(1.0)));
            assert!(assert_value_eq(&values[2], &Value::Number(2.0)));
        } else {
            panic!("Expected array result");
        }

        // 异常效应
        let code = r#"
            effect Exception {
                throw: (message: String) => Number
            }
            
            let divide = (a, b) => {
                if b == 0 {
                    perform Exception.throw("Division by zero")
                } else {
                    a / b
                }
            }
            
            let result = handle {
                let x = divide(10, 2)
                let y = divide(20, 0)
                let z = divide(30, 3)
                [x, y, z]
            } {
                effect Exception {
                    throw: (message) => {
                        resume(0)
                    }
                }
            }
            
            result
        "#;

        let result = execute_code(code).unwrap();
        if let Value::Array(values) = result {
            assert_eq!(values.len(), 3);
            assert!(assert_value_eq(&values[0], &Value::Number(5.0)));
            assert!(assert_value_eq(&values[1], &Value::Number(0.0)));
            assert!(assert_value_eq(&values[2], &Value::Number(10.0)));
        } else {
            panic!("Expected array result");
        }
    }

    #[test]
    fn test_pattern_matching() {
        // 简单模式匹配
        let code = r#"
            type Option<T> = | Some(T) | None()
            
            let value = Some(42)
            
            let result = match value {
                Some(x) => x * 2,
                None() => 0
            }
            
            result
        "#;

        let result = execute_code(code).unwrap();
        assert!(assert_value_eq(&result, &Value::Number(84.0)));

        // 复杂模式匹配
        let code = r#"
            type List<T> = | Nil() | Cons(T, List<T>)
            
            let list = Cons(1, Cons(2, Cons(3, Nil())))
            
            let sum = (list) => match list {
                Nil() => 0,
                Cons(head, tail) => head + sum(tail)
            }
            
            sum(list)
        "#;

        let result = execute_code(code).unwrap();
        assert!(assert_value_eq(&result, &Value::Number(6.0)));
    }

    #[test]
    fn test_type_system() {
        // 简单类型定义和使用
        let code = r#"
            type Point = {
                x: Number,
                y: Number
            }
            
            let createPoint = (x, y) => {
                x: x,
                y: y
            }
            
            let distance = (p1: Point, p2: Point) => {
                let dx = p2.x - p1.x
                let dy = p2.y - p1.y
                (dx * dx + dy * dy) ** 0.5
            }
            
            let p1 = createPoint(0, 0)
            let p2 = createPoint(3, 4)
            distance(p1, p2)
        "#;

        let result = execute_code(code).unwrap();
        assert!(assert_value_eq(&result, &Value::Number(5.0)));

        // 泛型类型
        let code = r#"
            type Pair<A, B> = {
                first: A,
                second: B
            }
            
            let makePair = <A, B>(a: A, b: B) => {
                first: a,
                second: b
            }
            
            let swap = <A, B>(pair: Pair<A, B>) => {
                first: pair.second,
                second: pair.first
            }
            
            let pair = makePair(42, "hello")
            let swapped = swap(pair)
            [swapped.first, swapped.second]
        "#;

        let result = execute_code(code).unwrap();
        if let Value::Array(values) = result {
            assert_eq!(values.len(), 2);
            assert!(assert_value_eq(
                &values[0],
                &Value::String("hello".to_string())
            ));
            assert!(assert_value_eq(&values[1], &Value::Number(42.0)));
        } else {
            panic!("Expected array result");
        }
    }

    #[test]
    fn test_error_handling() {
        // 使用 Option 类型处理错误
        let code = r#"
            type Option<T> = | Some(T) | None()
            
            let safeDivide = (a, b) => {
                if b == 0 {
                    None()
                } else {
                    Some(a / b)
                }
            }
            
            let result1 = safeDivide(10, 2)
            let result2 = safeDivide(10, 0)
            
            let getOrDefault = (opt, default) => match opt {
                Some(value) => value,
                None() => default
            }
            
            [getOrDefault(result1, 0), getOrDefault(result2, 0)]
        "#;

        let result = execute_code(code).unwrap();
        if let Value::Array(values) = result {
            assert_eq!(values.len(), 2);
            assert!(assert_value_eq(&values[0], &Value::Number(5.0)));
            assert!(assert_value_eq(&values[1], &Value::Number(0.0)));
        } else {
            panic!("Expected array result");
        }

        // 使用 Result 类型处理错误
        let code = r#"
            type Result<T, E> = | Ok(T) | Err(E)
            
            let safeDivide = (a, b) => {
                if b == 0 {
                    Err("Division by zero")
                } else {
                    Ok(a / b)
                }
            }
            
            let result1 = safeDivide(10, 2)
            let result2 = safeDivide(10, 0)
            
            let unwrapOr = (result, default) => match result {
                Ok(value) => value,
                Err(_) => default
            }
            
            [unwrapOr(result1, 0), unwrapOr(result2, 0)]
        "#;

        let result = execute_code(code).unwrap();
        if let Value::Array(values) = result {
            assert_eq!(values.len(), 2);
            assert!(assert_value_eq(&values[0], &Value::Number(5.0)));
            assert!(assert_value_eq(&values[1], &Value::Number(0.0)));
        } else {
            panic!("Expected array result");
        }
    }

    #[test]
    fn test_composition() {
        // 函数组合
        let code = r#"
            let compose = (f, g) => (x) => f(g(x))
            
            let double = (x) => x * 2
            let increment = (x) => x + 1
            
            let doubleAndIncrement = compose(increment, double)
            let incrementAndDouble = compose(double, increment)
            
            [doubleAndIncrement(5), incrementAndDouble(5)]
        "#;

        let result = execute_code(code).unwrap();
        if let Value::Array(values) = result {
            assert_eq!(values.len(), 2);
            assert!(assert_value_eq(&values[0], &Value::Number(11.0)));
            assert!(assert_value_eq(&values[1], &Value::Number(12.0)));
        } else {
            panic!("Expected array result");
        }
    }

    #[test]
    fn test_immutability() {
        // 不可变数据结构
        let code = r#"
            let update = (obj, key, value) => {
                let newObj = {}
                for (k in obj) {
                    newObj[k] = obj[k]
                }
                newObj[key] = value
                newObj
            }
            
            let user = {
                name: "Alice",
                age: 30
            }
            
            let updatedUser = update(user, "age", 31)
            
            [user.age, updatedUser.age]
        "#;

        let result = execute_code(code).unwrap();
        if let Value::Array(values) = result {
            assert_eq!(values.len(), 2);
            assert!(assert_value_eq(&values[0], &Value::Number(30.0)));
            assert!(assert_value_eq(&values[1], &Value::Number(31.0)));
        } else {
            panic!("Expected array result");
        }
    }
}
