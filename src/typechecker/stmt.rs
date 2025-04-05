use crate::ast::{Stmt, TypeDefinition};
use crate::typechecker::types::Type;
use crate::typechecker::error::TypeResult;
use crate::typechecker::checker::TypeChecker;
use std::collections::HashMap;

pub struct StmtTypeChecker<'a> {
    checker: &'a mut TypeChecker,
}

impl<'a> StmtTypeChecker<'a> {
    pub fn new(checker: &'a mut TypeChecker) -> Self {
        StmtTypeChecker { checker }
    }

    // 推导语句类型
    pub fn infer_stmt(&mut self, stmt: &Stmt) -> TypeResult<()> {
        match stmt {
            Stmt::FunctionDecl(name, params, body) => {
                // 创建新的类型环境用于函数定义
                let mut fn_env = self.checker.env.clone_with_non_generic();
                
                // 处理函数参数
                let mut param_types = Vec::new();
                for param in params {
                    let param_type = if let Some(annotation) = &param.type_annotation {
                        self.checker.convert_type_annotation(annotation)?
                    } else {
                        fn_env.new_type_var()
                    };
                    fn_env.add_var(param.name.clone(), param_type.clone());
                    param_types.push(param_type);
                }
                
                // 创建返回类型变量
                let return_type_var = fn_env.new_type_var();
                
                // 构造函数类型并提前添加到环境中，以支持递归
                let fn_type = Type::Function(param_types.clone(), Box::new(return_type_var.clone()));
                self.checker.env.add_var(name.clone(), fn_type.clone());
                fn_env.add_var(name.clone(), fn_type);
                
                // 推导函数体类型
                let mut fn_checker = TypeChecker {
                    env: fn_env,
                    subst: self.checker.subst.clone(),
                };
                let body_type = fn_checker.infer_expr(body)?;
                
                // 统一返回类型
                fn_checker.unify(&return_type_var, &body_type)?;
                
                // 更新替换
                self.checker.subst = fn_checker.subst;
                
                // 更新环境中的函数类型
                let final_return_type = self.checker.subst.apply(&body_type);
                let final_fn_type = Type::Function(param_types, Box::new(final_return_type));
                self.checker.env.add_var(name.clone(), final_fn_type);
                
                Ok(())
            }
            
            Stmt::VariableDecl(name, type_annotation, initializer) => self.infer_variable_decl(name, type_annotation, initializer),
            Stmt::TypeDecl(name, type_params, type_def) => self.infer_type_decl(name, type_params, type_def),
            Stmt::EffectDecl(name, _, _) => {
                // 简化处理效应定义
                Ok(())
            },
            Stmt::Expression(expr) => {
                self.checker.infer_expr(expr)?;
                Ok(())
            }
        }
    }

    // 推导函数声明
    fn infer_function_decl(&mut self, name: &str, params: &[crate::ast::Parameter], body: &crate::ast::Expr) -> TypeResult<()> {
        // 创建新的类型环境用于函数定义
        let mut fn_env = self.checker.env.clone_with_non_generic();
        let mut fn_checker = TypeChecker {
            env: fn_env,
            subst: self.checker.subst.clone(),
        };

        // 处理函数参数
        let mut param_types = Vec::new();
        for param in params {
            let param_type = if let Some(annotation) = &param.type_annotation {
                fn_checker.convert_type_annotation(annotation)?
            } else {
                fn_checker.env.new_type_var()
            };
            fn_checker.env.add_var(param.name.clone(), param_type.clone());
            param_types.push(param_type);
        }

        // 推导函数体类型
        let body_type = fn_checker.infer_expr(body)?;

        // 构造函数类型
        let fn_type = Type::Function(param_types, Box::new(body_type));
        self.checker.env.add_var(name.clone().to_string(), fn_type);

        // 更新替换
        self.checker.subst = fn_checker.subst;
        Ok(())
    }

    // 推导变量声明
    fn infer_variable_decl(&mut self, name: &str, type_annotation: &Option<crate::ast::TypeAnnotation>, initializer: &crate::ast::Expr) -> TypeResult<()> {
        let var_type = self.checker.infer_expr(initializer)?;
        
        // 如果有类型注解，检查它是否与推导类型匹配
        if let Some(annotation) = type_annotation {
            let annotated_type = self.checker.convert_type_annotation(annotation)?;
            self.checker.unify(&var_type, &annotated_type)?;
        }
        
        self.checker.env.add_var(name.clone().to_string(), var_type);
        Ok(())
    }

    // 推导类型声明
    fn infer_type_decl(&mut self, name: &str, type_params: &Option<Vec<String>>, type_def: &TypeDefinition) -> TypeResult<()> {
        // 处理类型定义
        let ty = Type::Generic(name.clone().to_string(), vec![]);
        self.checker.env.add_type(name.clone().to_string(), ty.clone());
        
        // 处理代数数据类型的构造器
        match type_def {
            TypeDefinition::Union(variants) => {
                for variant in variants {
                    // 获取构造器名称
                    let constructor_name = variant.name.clone();
                    
                    // 构造函数参数类型
                    let mut param_types = Vec::new();
                    if let Some(params) = &variant.params {
                        for param in params {
                            let param_type = if let Some(annotation) = &param.type_annotation {
                                self.checker.convert_type_annotation(annotation)?
                            } else {
                                Type::Any
                            };
                            param_types.push(param_type);
                        }
                    }
                    
                    // 创建构造器函数类型并添加到环境
                    let constructor_type = Type::Function(param_types, Box::new(ty.clone()));
                    self.checker.env.add_var(constructor_name, constructor_type);
                }
            },
            TypeDefinition::Record(fields) => {
                // 对于记录类型，可能需要不同的处理方式
                // 这里简单地注册类型名称，不创建构造器
            }
        }
        
        Ok(())
    }
}