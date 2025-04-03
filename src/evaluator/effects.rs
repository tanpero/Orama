use std::rc::Rc;
use std::cell::RefCell;
use crate::ast::{Expr, Pattern};
use crate::runtime::{Environment, Value, Function, RuntimeError, RuntimeResult, Effect};

// 效应处理上下文
pub struct EffectContext {
    pub effect_name: String,
    pub operation: String,
    pub args: Vec<Value>,
    pub resume: Rc<dyn Fn(Value) -> RuntimeResult<Value>>,
}

// 扩展 Evaluator 以处理效应
impl crate::evaluator::Evaluator {
    // 处理 perform 表达式
    pub fn evaluate_perform(&mut self, effect: &str, operation: &str, args: &[Expr]) -> RuntimeResult<Value> {
        // 评估参数
        let mut arg_values = Vec::new();
        for arg in args {
            arg_values.push(self.evaluate_expression(arg)?);
        }
        
        // 查找当前效应处理器栈
        if let Some(handler) = self.find_effect_handler(effect) {
            if let Some(op_handler) = handler.operations.get(operation) {
                // 创建恢复函数
                let resume_fn = Rc::new(move |value: Value| -> RuntimeResult<Value> {
                    // 恢复执行的逻辑
                    Ok(value)
                });
                
                // 创建效应上下文
                let context = EffectContext {
                    effect_name: effect.to_string(),
                    operation: operation.to_string(),
                    args: arg_values,
                    resume: resume_fn,
                };
                
                // 调用操作处理器
                self.call_operation_handler(op_handler, context)
            } else {
                Err(RuntimeError::Generic(format!("未定义的效应操作: {}.{}", effect, operation)))
            }
        } else {
            Err(RuntimeError::UnhandledEffect(effect.to_string(), operation.to_string()))
        }
    }
    
    // 处理 handle 表达式
    pub fn evaluate_handle(&mut self, expr: &Expr, handlers: &[EffectHandler], return_handler: &Option<ReturnHandler>) -> RuntimeResult<Value> {
        // 保存当前效应处理器栈
        let previous_handlers = self.effect_handlers.clone();
        
        // 添加新的效应处理器
        for handler in handlers {
            let mut operations = HashMap::new();
            for op in &handler.operations {
                let param_names: Vec<String> = op.params.iter().map(|p| p.name.clone()).collect();
                let function = Function {
                    params: param_names,
                    body: Rc::new(op.body.clone()),
                    closure: Rc::clone(&self.environment),
                };
                operations.insert(op.name.clone(), function);
            }
            
            let effect = Effect {
                name: handler.effect_name.clone(),
                operations,
            };
            
            self.effect_handlers.push(effect);
        }
        
        // 执行被处理的表达式
        let result = self.evaluate_expression(expr);
        
        // 恢复效应处理器栈
        self.effect_handlers = previous_handlers;
        
        // 应用返回处理器（如果有）
        if let Some(ret_handler) = return_handler {
            if let Ok(value) = result {
                // 创建新环境
                let previous_env = Rc::clone(&self.environment);
                self.environment = Rc::new(RefCell::new(Environment::with_enclosing(previous_env)));
                
                // 绑定返回值
                self.environment.borrow_mut().define(ret_handler.param.clone(), value);
                
                // 执行返回处理器
                let ret_result = self.evaluate_expression(&ret_handler.body);
                
                // 恢复环境
                self.environment = Rc::clone(&self.environment.borrow().enclosing.as_ref().unwrap());
                
                ret_result
            } else {
                result
            }
        } else {
            result
        }
    }
    
    // 查找效应处理器
    fn find_effect_handler(&self, effect_name: &str) -> Option<&Effect> {
        self.effect_handlers.iter().rev().find(|e| e.name == effect_name)
    }
    
    // 调用操作处理器
    fn call_operation_handler(&mut self, handler: &Function, context: EffectContext) -> RuntimeResult<Value> {
        // 创建新环境
        let previous_env = Rc::clone(&self.environment);
        self.environment = Rc::new(RefCell::new(Environment::with_enclosing(handler.closure.clone())));
        
        // 绑定参数
        for (i, param) in handler.params.iter().enumerate() {
            let arg = if i < context.args.len() {
                context.args[i].clone()
            } else {
                Value::Null
            };
            self.environment.borrow_mut().define(param.clone(), arg);
        }
        
        // 添加 resume 函数
        let resume_fn = context.resume.clone();
        let resume_function = Function {
            params: vec!["value".to_string()],
            body: Rc::new(Expr::Literal(Literal::Null)), // 占位符
            closure: Rc::clone(&self.environment),
        };
        self.environment.borrow_mut().define("resume".to_string(), Value::Function(resume_function));
        
        // 执行处理器体
        let result = self.evaluate_expression(&handler.body);
        
        // 恢复环境
        self.environment = previous_env;
        
        result
    }
    
    // 处理 match 表达式
    pub fn evaluate_match(&mut self, expr: &Expr, cases: &[MatchCase]) -> RuntimeResult<Value> {
        let value = self.evaluate_expression(expr)?;
        
        for case in cases {
            if let Some(bindings) = self.match_pattern(&case.pattern, &value) {
                // 创建新环境
                let previous_env = Rc::clone(&self.environment);
                self.environment = Rc::new(RefCell::new(Environment::with_enclosing(previous_env)));
                
                // 绑定匹配的变量
                for (name, val) in bindings {
                    self.environment.borrow_mut().define(name, val);
                }
                
                // 执行匹配分支
                let result = self.evaluate_expression(&case.body);
                
                // 恢复环境
                self.environment = Rc::clone(&self.environment.borrow().enclosing.as_ref().unwrap());
                
                return result;
            }
        }
        
        Err(RuntimeError::Generic("模式匹配失败".to_string()))
    }
    
    // 匹配模式
    fn match_pattern(&self, pattern: &Pattern, value: &Value) -> Option<HashMap<String, Value>> {
        let mut bindings = HashMap::new();
        
        // 简单的变量模式
        if pattern.params.is_none() {
            bindings.insert(pattern.name.clone(), value.clone());
            return Some(bindings);
        }
        
        // 构造器模式
        if let Value::Object(fields) = value {
            if let Some(constructor) = fields.get("constructor") {
                if let Value::String(constructor_name) = constructor {
                    if constructor_name == &pattern.name {
                        if let Some(params) = &pattern.params {
                            if let Some(args) = fields.get("args") {
                                if let Value::Array(arg_values) = args {
                                    if params.len() == arg_values.len() {
                                        for (i, param) in params.iter().enumerate() {
                                            if let Some(sub_bindings) = self.match_pattern(param, &arg_values[i]) {
                                                bindings.extend(sub_bindings);
                                            } else {
                                                return None;
                                            }
                                        }
                                        return Some(bindings);
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }
        
        None
    }
}