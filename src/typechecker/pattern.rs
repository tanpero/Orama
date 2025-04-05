use crate::ast::Pattern;
use crate::typechecker::types::Type;
use crate::typechecker::error::{TypeError, TypeResult};
use crate::typechecker::checker::TypeChecker;

pub struct PatternTypeChecker<'a> {
    checker: &'a mut TypeChecker,
}

impl<'a> PatternTypeChecker<'a> {
    pub fn new(checker: &'a mut TypeChecker) -> Self {
        PatternTypeChecker { checker }
    }

    // 推导模式类型
    pub fn infer_pattern(&mut self, pattern: &Pattern, value_type: &Type) -> TypeResult<()> {
        // 简单变量模式
        if pattern.params.is_none() {
            self.checker.env.add_var(pattern.name.clone(), value_type.clone());
            return Ok(());
        }
        
        // 构造器模式
        if let Some(params) = &pattern.params {
            // 检查值类型是否为代数数据类型
            if let Type::Generic(type_name, _) = value_type {
                // 获取构造器名称
                let constructor_name = &pattern.name;
                
                // 查找构造器函数 - 先获取并克隆构造器类型，避免后续借用冲突
                let constructor_type_opt = self.checker.env.get_var(constructor_name).cloned();
                
                if let Some(constructor_type) = constructor_type_opt {
                    // 检查构造器是否返回正确的类型
                    if let Type::Function(param_types, ret_type) = constructor_type {
                        // 检查返回类型是否与值类型匹配
                        if let Type::Generic(ret_name, _) = ret_type.as_ref() {
                            if ret_name == type_name {
                                // 构造器匹配成功，处理参数
                                if params.len() == param_types.len() {
                                    // 为每个参数绑定类型 - 先克隆所有需要的类型
                                    let param_bindings: Vec<(String, Type)> = params.iter().enumerate()
                                        .map(|(i, param)| (param.name.clone(), param_types[i].clone()))
                                        .collect();
                                    
                                    // 现在可以安全地添加变量，因为不再有活跃的不可变借用
                                    for (name, param_type) in param_bindings {
                                        self.checker.env.add_var(name, param_type);
                                    }
                                    return Ok(());
                                }
                            }
                        }
                    }
                }
            }
            
            // 如果上面的匹配失败，尝试更通用的方法
            // 这里假设值类型可能是一个记录类型，表示一个代数数据类型的实例
            if let Type::Record(fields) = value_type {
                // 检查是否有构造器字段
                if let Some(constructor) = fields.get("constructor") {
                    if let Type::String = constructor {
                        // 检查是否有args字段
                        if let Some(args) = fields.get("args") {
                            if let Type::Array(elem_type) = args {
                                // 为每个模式参数绑定类型
                                for (i, param) in params.iter().enumerate() {
                                    // 如果我们不知道具体类型，使用Any
                                    let param_type = Type::Any;
                                    self.checker.env.add_var(param.name.clone(), param_type);
                                }
                                return Ok(());
                            }
                        }
                    }
                }
            }
            
            return Err(TypeError::TypeMismatch {
                expected: format!("构造器 {}", pattern.name),
                actual: format!("{}", value_type),
            });
        }
        
        Ok(())
    }
}