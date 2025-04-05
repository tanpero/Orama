use crate::ast::TypeDefinition;
use crate::ast::{BinaryOp, Expr, Literal, Program, Stmt, UnaryOp};
use crate::runtime::Effect;
use crate::runtime::{Environment, Function, RuntimeError, RuntimeResult, Value};
use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

pub struct Evaluator {
    environment: Rc<RefCell<Environment>>,
    effect_handlers: Vec<Effect>,
}

impl Evaluator {
    pub fn new() -> Self {
        Evaluator {
            environment: Rc::new(RefCell::new(Environment::new())),
            effect_handlers: Vec::new(),
        }
    }

    pub fn with_environment(env: Rc<RefCell<Environment>>) -> Self {
        Evaluator {
            environment: env,
            effect_handlers: Vec::new(),
        }
    }

    pub fn evaluate(&mut self, program: &Program) -> RuntimeResult<Value> {
        let mut result = Value::Null;

        for stmt in &program.statements {
            result = self.evaluate_statement(stmt)?;
        }

        Ok(result)
    }

    fn evaluate_statement(&mut self, stmt: &Stmt) -> RuntimeResult<Value> {
        match stmt {
            Stmt::VariableDecl(name, type_annotation, expr) => {
                let value = self.evaluate_expression(expr)?;
                self.environment.borrow_mut().define(name.clone(), value);
                Ok(Value::Null)
            }
            Stmt::FunctionDecl(name, params, body) => {
                let param_names: Vec<String> = params.iter().map(|p| p.name.clone()).collect();
                let function = Function {
                    param_types: Vec::new(), // 添加缺失的字段
                    return_type: None,       // 添加缺失的字段
                    params: param_names,
                    body: Rc::new(body.clone()),
                    closure: Rc::clone(&self.environment),
                };

                self.environment
                    .borrow_mut()
                    .define(name.clone(), Value::Function(function));
                Ok(Value::Null)
            }
            Stmt::Expression(expr) => self.evaluate_expression(expr),
            // 处理类型声明，为每个变体创建构造函数
            Stmt::TypeDecl(name, _, type_def) => {
                // 根据TypeDefinition的类型处理
                match type_def {
                    TypeDefinition::Union(variants) => {
                        // 为联合类型的每个变体创建构造函数
                        for variant in variants {
                            let variant_name = &variant.name;
                            let param_count = if let Some(params) = &variant.params {
                                params.len()
                            } else {
                                0
                            };

                            // 创建参数名称列表
                            let param_names: Vec<String> =
                                (0..param_count).map(|i| format!("p{}", i)).collect();

                            // 创建构造函数体 - 返回一个表示该变体的对象
                            let body = Expr::Literal(Literal::Object(vec![
                                (
                                    "constructor".to_string(),
                                    Expr::Literal(Literal::String(variant_name.clone())),
                                ),
                                (
                                    "type".to_string(),
                                    Expr::Literal(Literal::String(name.clone())),
                                ),
                                (
                                    "args".to_string(),
                                    Expr::Literal(Literal::Array(
                                        param_names
                                            .iter()
                                            .map(|p| Expr::Variable(p.clone()))
                                            .collect(),
                                    )),
                                ),
                            ]));

                            // 创建构造函数
                            let function = Function {
                                param_types: Vec::new(), // 添加缺失的字段
                                return_type: None,       // 添加缺失的字段
                                params: param_names,
                                body: Rc::new(body),
                                closure: Rc::clone(&self.environment),
                            };

                            // 将构造函数添加到环境中
                            self.environment
                                .borrow_mut()
                                .define(variant_name.clone(), Value::Function(function));
                        }
                    }
                    TypeDefinition::Record(_) => {
                        // 记录类型暂时不需要特殊处理
                        // 如果需要为记录类型创建构造函数，可以在这里添加代码
                    }
                }

                Ok(Value::Null)
            }
            Stmt::EffectDecl(_, _, _) => Ok(Value::Null),
        }
    }

    pub fn evaluate_expression(&mut self, expr: &Expr) -> RuntimeResult<Value> {
        match expr {
            Expr::Literal(lit) => self.evaluate_literal(lit),
            Expr::Variable(name) => {
                if let Some(value) = self.environment.borrow().get(name) {
                    Ok(value)
                } else {
                    Err(RuntimeError::UndefinedVariable(name.clone()))
                }
            }
            Expr::Binary(left, op, right) => {
                let left_val = self.evaluate_expression(left)?;
                let right_val = self.evaluate_expression(right)?;
                self.evaluate_binary_op(&left_val, op, &right_val)
            }
            Expr::Unary(op, expr) => {
                let value = self.evaluate_expression(expr)?;
                self.evaluate_unary_op(op, &value)
            }
            Expr::Call(callee, args) => {
                let callee_val = self.evaluate_expression(callee)?;

                let mut arg_values = Vec::new();
                for arg in args {
                    arg_values.push(self.evaluate_expression(arg)?);
                }

                self.call_function(callee_val, arg_values)
            }
            Expr::Function(params, body) => {
                let param_names: Vec<String> = params.iter().map(|p| p.name.clone()).collect();
                let function = Function {
                    param_types: Vec::new(), // 初始化参数类型为空向量
                    return_type: None,       // 初始化返回类型为 None
                    params: param_names,
                    body: Rc::new(*body.clone()),
                    closure: Rc::clone(&self.environment),
                };

                Ok(Value::Function(function))
            }
            Expr::If(condition, then_branch, else_branch) => {
                let condition_val = self.evaluate_expression(condition)?;

                if self.is_truthy(&condition_val) {
                    self.evaluate_expression(then_branch)
                } else if let Some(else_expr) = else_branch {
                    self.evaluate_expression(else_expr)
                } else {
                    Ok(Value::Null)
                }
            }
            Expr::Block(stmts, expr) => {
                // 创建新的环境
                let previous_env = Rc::clone(&self.environment);
                self.environment = Rc::new(RefCell::new(Environment::with_enclosing(previous_env)));

                // 执行语句
                for stmt in stmts {
                    self.evaluate_statement(stmt)?;
                }

                // 执行可选的表达式
                let result = if let Some(expr) = expr {
                    self.evaluate_expression(expr)?
                } else {
                    Value::Null
                };

                // 恢复环境
                // 先获取当前环境的引用
                let current_env = self.environment.borrow();
                // 获取外层环境
                let enclosing = current_env.get_enclosing();
                // 释放当前环境的借用
                drop(current_env);
                // 设置新环境
                self.environment = Rc::clone(&enclosing.as_ref().unwrap());

                Ok(result)
            }
            Expr::Pipe(left, right) => {
                let left_val = self.evaluate_expression(left)?;

                // 对于管道操作，我们需要将左侧的值作为右侧函数的第一个参数
                if let Expr::Call(callee, mut args) = right.as_ref().clone() {
                    // 在参数列表前插入左侧的值
                    let mut new_args = vec![Expr::Literal(self.value_to_literal(&left_val)?)];
                    new_args.extend(args);

                    // 创建新的函数调用表达式
                    let new_call = Expr::Call(callee, new_args);
                    self.evaluate_expression(&new_call)
                } else {
                    // 如果右侧不是函数调用，则尝试将其作为函数直接调用
                    let right_val = self.evaluate_expression(right)?;
                    self.call_function(right_val, vec![left_val])
                }
            }
            // 删除这里错误放置的索引操作代码
            // 暂时忽略其他表达式类型
            Expr::Perform(_, _, _) => Ok(Value::Null),
            Expr::Handle(_, _, _) => Ok(Value::Null),
            Expr::Match(value, cases) => {
                let value = self.evaluate_expression(value)?;

                for case in cases {
                    if let Some(bindings) = self.match_pattern(&case.pattern, &value) {
                        // 创建新环境
                        let previous_env = Rc::clone(&self.environment);
                        self.environment =
                            Rc::new(RefCell::new(Environment::with_enclosing(previous_env)));

                        // 绑定匹配的变量
                        for (name, val) in bindings {
                            self.environment.borrow_mut().define(name, val);
                        }

                        // 执行匹配分支
                        let result = self.evaluate_expression(&case.body);

                        // 恢复环境
                        let current_env = self.environment.borrow();
                        let enclosing = current_env.get_enclosing();
                        drop(current_env);
                        self.environment = Rc::clone(&enclosing.as_ref().unwrap());

                        return result;
                    }
                }

                Err(RuntimeError::Generic("模式匹配失败".to_string()))
            }
        }
    }

    // 添加模式匹配辅助方法
    fn match_pattern(
        &self,
        pattern: &crate::ast::Pattern,
        value: &Value,
    ) -> Option<HashMap<String, Value>> {
        let mut bindings = HashMap::new();

        // 检查是否是构造器模式
        if let Value::Object(fields) = value {
            if let Some(constructor) = fields.get("constructor") {
                if let Value::String(constructor_name) = constructor {
                    if constructor_name == &pattern.name {
                        if let Some(args) = fields.get("args") {
                            if let Value::Array(arg_values) = args {
                                // 检查参数数量是否匹配
                                if let Some(pattern_params) = &pattern.params {
                                    if pattern_params.len() == arg_values.len() {
                                        // 递归匹配每个参数
                                        for (i, param) in pattern_params.iter().enumerate() {
                                            if let Some(param_bindings) =
                                                self.match_pattern(param, &arg_values[i])
                                            {
                                                bindings.extend(param_bindings);
                                            } else {
                                                return None;
                                            }
                                        }
                                        return Some(bindings);
                                    }
                                } else if arg_values.is_empty() {
                                    // 无参数构造器
                                    return Some(bindings);
                                }
                            }
                        }
                    }
                }
            }
        }

        // 变量模式 - 直接绑定整个值
        if pattern.params.is_none() {
            bindings.insert(pattern.name.clone(), value.clone());
            return Some(bindings);
        }

        None
    }
    fn evaluate_literal(&mut self, lit: &Literal) -> RuntimeResult<Value> {
        match lit {
            Literal::Unit => Ok(Value::Unit),
            Literal::Number(n) => Ok(Value::Number(*n)),
            Literal::String(s) => Ok(Value::String(s.clone())),
            Literal::Boolean(b) => Ok(Value::Boolean(*b)),
            Literal::Array(items) => {
                let mut values = Vec::new();
                for item in items {
                    values.push(self.evaluate_expression(item)?);
                }
                Ok(Value::Array(values))
            }
            Literal::Object(fields) => {
                let mut map = HashMap::new();
                for (key, value) in fields {
                    map.insert(key.clone(), self.evaluate_expression(value)?);
                }
                Ok(Value::Object(map))
            }
            Literal::Null => Ok(Value::Null),
        }
    }

    fn evaluate_binary_op(&self, left: &Value, op: &BinaryOp, right: &Value) -> RuntimeResult<Value> {
        match (left, op, right) {
            // 数值运算
            (Value::Number(l), BinaryOp::Add, Value::Number(r)) => Ok(Value::Number(l + r)),
            (Value::Number(l), BinaryOp::Subtract, Value::Number(r)) => Ok(Value::Number(l - r)),
            (Value::Number(l), BinaryOp::Multiply, Value::Number(r)) => Ok(Value::Number(l * r)),
            (Value::Number(l), BinaryOp::Divide, Value::Number(r)) => {
                if *r == 0.0 {
                    Err(RuntimeError::Generic("除以零错误".to_string()))
                } else {
                    Ok(Value::Number(l / r))
                }
            }
            (Value::Number(l), BinaryOp::Modulo, Value::Number(r)) => {
                if *r == 0.0 {
                    Err(RuntimeError::Generic("模零错误".to_string()))
                } else {
                    Ok(Value::Number(l % r))
                }
            }

            // 字符串连接
            (Value::String(l), BinaryOp::Add, Value::String(r)) => Ok(Value::String(l.clone() + r)),

            // 比较运算
            (Value::Number(l), BinaryOp::Equal, Value::Number(r)) => Ok(Value::Boolean(l == r)),
            (Value::Number(l), BinaryOp::NotEqual, Value::Number(r)) => Ok(Value::Boolean(l != r)),
            (Value::Number(l), BinaryOp::Less, Value::Number(r)) => Ok(Value::Boolean(l < r)),
            (Value::Number(l), BinaryOp::Greater, Value::Number(r)) => Ok(Value::Boolean(l > r)),
            (Value::Number(l), BinaryOp::LessEqual, Value::Number(r)) => Ok(Value::Boolean(l <= r)),
            (Value::Number(l), BinaryOp::GreaterEqual, Value::Number(r)) => {
                Ok(Value::Boolean(l >= r))
            }

            // 逻辑运算
            (Value::Boolean(l), BinaryOp::And, Value::Boolean(r)) => Ok(Value::Boolean(*l && *r)),
            (Value::Boolean(l), BinaryOp::Or, Value::Boolean(r)) => Ok(Value::Boolean(*l || *r)),

            // 添加对象和数组索引操作
            (Value::Object(obj), BinaryOp::Index, Value::String(key)) => {
                if let Some(value) = obj.get(key) {
                    Ok(value.clone())
                } else {
                    Err(RuntimeError::Generic(format!("对象中不存在键 '{}'", key)))
                }
            }
            (Value::Array(arr), BinaryOp::Index, Value::Number(idx)) => {
                let index = *idx as usize;
                if index < arr.len() {
                    Ok(arr[index].clone())
                } else {
                    Err(RuntimeError::Generic(format!(
                        "索引越界: {} (数组长度: {})",
                        index,
                        arr.len()
                    )))
                }
            }

            // 类型错误
            _ => Err(RuntimeError::TypeError(format!(
                "不支持的操作: {:?} {:?} {:?}",
                left, op, right
            ))),
        }
    }

    fn evaluate_unary_op(&self, op: &UnaryOp, value: &Value) -> RuntimeResult<Value> {
        match (op, value) {
            (UnaryOp::Negate, Value::Number(n)) => Ok(Value::Number(-n)),
            (UnaryOp::Not, Value::Boolean(b)) => Ok(Value::Boolean(!b)),
            _ => Err(RuntimeError::TypeError(format!(
                "不支持的一元操作: {:?} {:?}",
                op, value
            ))),
        }
    }

    pub fn call_function(&mut self, callee: Value, args: Vec<Value>) -> RuntimeResult<Value> {
        match callee {
            Value::Function(function) => {
                if function.params.len() != args.len() {
                    return Err(RuntimeError::ArgumentMismatch {
                        expected: function.params.len(),
                        actual: args.len(),
                    });
                }

                // 创建新的环境，包含函数的闭包环境
                let previous_env = self.environment.clone();
                let env = Environment::with_enclosing(function.closure);
                self.environment = Rc::new(RefCell::new(env));

                // 绑定参数
                for (param, arg) in function.params.iter().zip(args) {
                    self.environment.borrow_mut().define(param.clone(), arg);
                }

                // 执行函数体
                let result = self.evaluate_expression(&function.body)?;

                // 恢复环境
                self.environment = previous_env;

                Ok(result)
            }
            Value::NativeFunction(native_fn) => {
                if native_fn.params.len() != args.len() {
                    return Err(RuntimeError::ArgumentMismatch {
                        expected: native_fn.params.len(),
                        actual: args.len(),
                    });
                }

                // 调用原生函数
                (native_fn.func)(args, Rc::clone(&self.environment))
            }
            _ => Err(RuntimeError::TypeError("不是一个可调用的值".to_string())),
        }
    }

    fn is_truthy(&self, value: &Value) -> bool {
        match value {
            Value::Unit => true,
            Value::Boolean(b) => *b,
            Value::Null => false,
            Value::Number(n) => *n != 0.0,
            Value::String(s) => !s.is_empty(),
            Value::Array(a) => !a.is_empty(),
            Value::Object(_) => true,
            Value::Function(_) => true,
            Value::Effect(_) => true,
            Value::NativeFunction(_) => true, // Add this missing case
        }
    }

    fn value_to_literal(&self, value: &Value) -> RuntimeResult<Literal> {
        match value {
            Value::Number(n) => Ok(Literal::Number(*n)),
            Value::String(s) => Ok(Literal::String(s.clone())),
            Value::Boolean(b) => Ok(Literal::Boolean(*b)),
            Value::Null => Ok(Literal::Null),
            Value::Array(items) => {
                let mut literals = Vec::new();
                for item in items {
                    literals.push(Expr::Literal(self.value_to_literal(item)?));
                }
                Ok(Literal::Array(literals))
            }
            Value::Object(fields) => {
                let mut literal_fields = HashMap::new();
                for (key, value) in fields {
                    literal_fields
                        .insert(key.clone(), Expr::Literal(self.value_to_literal(value)?));
                }
                // In the function where you're creating the Literal::Object
                // Replace:
                // Ok(Literal::Object(literal_fields))
                // With:
                let object_fields: Vec<(String, Expr)> =
                    literal_fields.into_iter().map(|(k, v)| (k, v)).collect();
                Ok(Literal::Object(object_fields))
            }
            _ => Err(RuntimeError::TypeError(
                "无法将此值转换为字面量".to_string(),
            )),
        }
    }
}
