use crate::ast::{Expr, Literal, TypeAnnotation, BinaryOp, UnaryOp, Stmt, Parameter, Pattern, MatchCase};
use crate::typechecker::types::{Type, TypeVarId};
use crate::typechecker::env::TypeEnv;
use crate::typechecker::subst::TypeSubst;
use crate::typechecker::error::{TypeError, TypeResult};
use std::collections::HashMap;
use crate::ast::TypeDefinition;
use crate::runtime::Value;
use std::rc::Rc;
use std::cell::RefCell;
use crate::runtime::Environment;

// 类型检查器
pub struct TypeChecker {
    pub env: TypeEnv,
    pub subst: TypeSubst,
}

impl TypeChecker {
    pub fn new() -> Self {
        TypeChecker {
            env: TypeEnv::new(),
            subst: TypeSubst::new(),
        }
    }

    // 初始化内置类型
    pub fn init_builtins(&mut self) {
        // 添加基本类型
        self.env.add_type("Number".to_string(), Type::Number);
        self.env.add_type("String".to_string(), Type::String);
        self.env.add_type("Boolean".to_string(), Type::Boolean);
        self.env.add_type("Any".to_string(), Type::Any);
        self.env.add_type("Null".to_string(), Type::Null);
    }

    // 从 AST 类型注解转换为内部类型表示
    pub fn convert_type_annotation(&mut self, annotation: &TypeAnnotation) -> TypeResult<Type> {
        match annotation {
            TypeAnnotation::Simple(name, type_args) => {
                let args = if let Some(args) = type_args {
                    args.iter()
                        .map(|arg| self.convert_type_annotation(arg))
                        .collect::<Result<Vec<_>, _>>()?
                } else {
                    vec![]
                };

                if let Some(ty) = self.env.get_type(name) {
                    if args.is_empty() {
                        Ok(ty.clone())
                    } else {
                        match ty {
                            Type::Generic(_, _) => Ok(Type::Generic(name.clone(), args)),
                            _ => Err(TypeError::TypeMismatch {
                                expected: format!("泛型类型 {}", name),
                                actual: format!("{}", ty),
                            }),
                        }
                    }
                } else {
                    // 未知类型，假设是泛型
                    Ok(Type::Generic(name.clone(), args))
                }
            }
            TypeAnnotation::Function(fn_type) => {
                let param_types = fn_type.params
                    .iter()
                    .map(|param| self.convert_type_annotation(param))
                    .collect::<Result<Vec<_>, _>>()?;
                
                let return_type = Box::new(self.convert_type_annotation(&fn_type.return_type)?);
                
                Ok(Type::Function(param_types, return_type))
            }
            TypeAnnotation::Effect(_, return_type) => {
                // 简化处理，忽略效应列表
                self.convert_type_annotation(return_type)
            }
            TypeAnnotation::Array(elem_type) => {
                let elem = Box::new(self.convert_type_annotation(elem_type)?);
                Ok(Type::Array(elem))
            }
        }
    }

    // 统一两个类型
    pub fn unify(&mut self, t1: &Type, t2: &Type) -> TypeResult<()> {
        let t1 = self.subst.apply(t1);
        let t2 = self.subst.apply(t2);

        match (&t1, &t2) {
            // 相同类型直接匹配
            (a, b) if a == b => Ok(()),

            // 类型变量的情况
            (Type::Var(id), ty) | (ty, Type::Var(id)) => {
                self.unify_var(*id, ty.clone())
            }

            // 数组类型
            (Type::Array(a), Type::Array(b)) => {
                self.unify(a, b)
            }

            // 函数类型
            (Type::Function(params1, ret1), Type::Function(params2, ret2)) => {
                if params1.len() != params2.len() {
                    return Err(TypeError::UnificationFailure(
                        format!("{}", t1),
                        format!("{}", t2),
                    ));
                }

                for (p1, p2) in params1.iter().zip(params2.iter()) {
                    self.unify(p1, p2)?;
                }

                self.unify(ret1, ret2)
            }

            // 记录类型
            (Type::Record(fields1), Type::Record(fields2)) => {
                if fields1.len() != fields2.len() {
                    return Err(TypeError::UnificationFailure(
                        format!("{}", t1),
                        format!("{}", t2),
                    ));
                }

                for (name, ty1) in fields1 {
                    if let Some(ty2) = fields2.get(name) {
                        self.unify(ty1, ty2)?;
                    } else {
                        return Err(TypeError::UnificationFailure(
                            format!("{}", t1),
                            format!("{}", t2),
                        ));
                    }
                }

                Ok(())
            }

            (Type::Generic(name1, args1), Type::Generic(name2, args2)) => {
                if name1 != name2 || args1.len() != args2.len() {
                    return Err(TypeError::UnificationFailure(
                        format!("{}", t1),
                        format!("{}", t2),
                    ));
                }
                
                for (a1, a2) in args1.iter().zip(args2.iter()) {
                    self.unify(a1, a2)?;
                }
                
                Ok(())
            }

            // Any 类型可以与任何类型匹配
            (Type::Any, _) | (_, Type::Any) => Ok(()),

            // 其他情况无法统一
            _ => Err(TypeError::UnificationFailure(
                format!("{}", t1),
                format!("{}", t2),
            )),
        }
    }

    // 统一类型变量与类型
    fn unify_var(&mut self, var_id: TypeVarId, ty: Type) -> TypeResult<()> {
        // 检查是否已有替换
        if let Some(existing) = self.subst.0.get(&var_id) {
            // 克隆现有类型以避免可变和不可变借用冲突
            let existing_type = existing.clone();
            return self.unify(&existing_type, &ty);
        }

        // 检查是否出现在类型中（防止递归类型）
        if Self::occurs_check(var_id, &ty) {
            return Err(TypeError::RecursiveType);
        }

        // 添加替换
        self.subst.add(var_id, ty);
        Ok(())
    }

    // 检查类型变量是否出现在类型中
    fn occurs_check(var_id: TypeVarId, ty: &Type) -> bool {
        match ty {
            Type::Var(id) => *id == var_id,
            Type::Array(elem) => Self::occurs_check(var_id, elem),
            Type::Function(params, ret) => {
                params.iter().any(|p| Self::occurs_check(var_id, p)) || 
                Self::occurs_check(var_id, ret)
            }
            Type::Record(fields) => {
                fields.values().any(|f| Self::occurs_check(var_id, f))
            }
            Type::Generic(_, args) => {
                args.iter().any(|a| Self::occurs_check(var_id, a))
            }
            _ => false,
        }
    }

    // 检查函数调用与函数签名是否匹配
    fn check_function_call(&mut self, fn_type: &Type, args: &[Expr]) -> TypeResult<Type> {
        match fn_type {
            Type::Function(param_types, return_type) => {
                if args.len() != param_types.len() {
                    return Err(TypeError::TypeMismatch {
                        expected: format!("函数参数数量: {}", param_types.len()),
                        actual: format!("提供的参数数量: {}", args.len()),
                    });
                }
                
                // 检查每个参数类型
                for (i, (arg, expected_type)) in args.iter().zip(param_types.iter()).enumerate() {
                    let arg_type = self.infer_expr(arg)?;
                    if let Err(e) = self.unify(&arg_type, expected_type) {
                        return Err(TypeError::TypeMismatch {
                            expected: format!("参数 #{}: {}", i + 1, expected_type),
                            actual: format!("参数 #{}: {}", i + 1, arg_type),
                        });
                    }
                }
                
                Ok(self.subst.apply(return_type))
            },
            _ => Err(TypeError::TypeMismatch {
                expected: "函数类型".to_string(),
                actual: format!("{}", fn_type),
            }),
        }
    }

    // 推导表达式类型
    pub fn infer_expr(&mut self, expr: &Expr) -> TypeResult<Type> {
        match expr {
            Expr::Literal(lit) => self.infer_literal(lit),
            Expr::Variable(name) => {
                if let Some(ty) = self.env.get_var(name) {
                    Ok(ty.clone())
                } else {
                    Err(TypeError::UndefinedVariable(name.clone()))
                }
            }
            Expr::Binary(left, op, right) => {
                let left_type = self.infer_expr(left)?;
                let right_type = self.infer_expr(right)?;

                match op {
                    BinaryOp::Index => {
                        let left_type = self.infer_expr(left)?;
                        let right_type = self.infer_expr(right)?;
                        
                        // 检查右侧是否为字符串（对象键）
                        self.unify(&right_type, &Type::String)?;
                        
                        // 对于对象类型，我们需要检查是否为记录类型
                        if let Type::Record(fields) = &left_type {
                            // 如果是字符串字面量，我们可以直接检查字段
                            if let Expr::Literal(Literal::String(field_name)) = &**right {
                                if let Some(field_type) = fields.get(field_name) {
                                    return Ok(field_type.clone());
                                }
                            }
                            
                            // 如果不是字面量或字段不存在，返回 Any 类型
                            // 这允许动态访问，但在运行时可能会失败
                            Ok(Type::Any)
                        } else {
                            // 如果不是记录类型，可能是数组
                            if let Type::Array(elem_type) = &left_type {
                                // 检查索引是否为数字
                                self.unify(&right_type, &Type::Number)?;
                                return Ok(*elem_type.clone());
                            }
                            
                            // 如果既不是记录也不是数组，报错
                            Err(TypeError::TypeMismatch {
                                expected: "对象或数组".to_string(),
                                actual: format!("{}", left_type),
                            })
                        }
                    },
                    // 访问运算符
                    BinaryOp::Access => {
                        // 确保左侧是记录类型
                        if let Type::Record(fields) = &left_type {
                            // 右侧必须是字符串字面量
                            if let Expr::Literal(Literal::String(field_name)) = &**right {
                                if let Some(field_type) = fields.get(field_name) {
                                    Ok(field_type.clone())
                                } else {
                                    Err(TypeError::UnificationFailure(
                                        format!("记录中不存在字段 {}", field_name),
                                        format!("{}", left_type),
                                    ))
                                }
                            } else {
                                Err(TypeError::TypeMismatch {
                                    expected: "字符串字面量".to_string(),
                                    actual: format!("{}", right_type),
                                })
                            }
                        } else {
                            Err(TypeError::TypeMismatch {
                                expected: "记录类型".to_string(),
                                actual: format!("{}", left_type),
                            })
                        }
                    }
                    // 加法运算符 - 特殊处理，支持数字加法和字符串连接
                    BinaryOp::Add => {
                        // 尝试作为数字处理
                        if let (Type::Number, Type::Number) = (&left_type, &right_type) {
                            return Ok(Type::Number);
                        }
                        
                        // 尝试作为字符串处理
                        if let (Type::String, Type::String) = (&left_type, &right_type) {
                            return Ok(Type::String);
                        }
                        
                        // 如果左右类型相同，统一它们
                        self.unify(&left_type, &right_type)?;
                        
                        // 根据统一后的类型决定结果类型
                        let unified_type = self.subst.apply(&left_type);
                        match unified_type {
                            Type::Number => Ok(Type::Number),
                            Type::String => Ok(Type::String),
                            _ => Err(TypeError::TypeMismatch {
                                expected: "Number 或 String".to_string(),
                                actual: format!("{}", unified_type),
                            })
                        }
                    }
                    // 算术运算符
                    BinaryOp::Subtract | BinaryOp::Multiply | 
                    BinaryOp::Divide | BinaryOp::Modulo => {
                        self.unify(&left_type, &Type::Number)?;
                        self.unify(&right_type, &Type::Number)?;
                        Ok(Type::Number)
                    }
                    // 比较运算符
                    BinaryOp::Equal | BinaryOp::NotEqual | 
                    BinaryOp::Less | BinaryOp::LessEqual | 
                    BinaryOp::Greater | BinaryOp::GreaterEqual => {
                        // 比较运算符要求两边类型相同，但不限制具体类型
                        self.unify(&left_type, &right_type)?;
                        Ok(Type::Boolean)
                    }
                    // 逻辑运算符
                    BinaryOp::And | BinaryOp::Or => {
                        self.unify(&left_type, &Type::Boolean)?;
                        self.unify(&right_type, &Type::Boolean)?;
                        Ok(Type::Boolean)
                    }
                }
            }
            Expr::Unary(op, expr) => {
                let expr_type = self.infer_expr(expr)?;

                match op {
                    UnaryOp::Negate => {
                        self.unify(&expr_type, &Type::Number)?;
                        Ok(Type::Number)
                    }
                    UnaryOp::Not => {
                        self.unify(&expr_type, &Type::Boolean)?;
                        Ok(Type::Boolean)
                    }
                }
            }
            Expr::Call(callee, args) => {
                let callee_type = self.infer_expr(callee)?;
                self.check_function_call(&callee_type, args)
            }
            Expr::Function(params, body) => {
                // 创建新的类型环境
                let mut fn_env = self.env.clone_with_non_generic();
                
                // 为每个参数创建类型变量
                let mut param_types = Vec::new();
                for param in params {
                    let param_type = if let Some(annotation) = &param.type_annotation {
                        self.convert_type_annotation(annotation)?
                    } else {
                        fn_env.new_type_var()
                    };
                    
                    fn_env.add_var(param.name.clone(), param_type.clone());
                    param_types.push(param_type);
                }
                
                // 推导函数体类型
                let mut fn_checker = TypeChecker {
                    env: fn_env,
                    subst: self.subst.clone(),
                };
                let body_type = fn_checker.infer_expr(body)?;
                
                // 更新替换
                self.subst = fn_checker.subst;
                
                Ok(Type::Function(param_types, Box::new(body_type)))
            }
            Expr::If(condition, then_branch, else_branch) => {
                let cond_type = self.infer_expr(condition)?;
                self.unify(&cond_type, &Type::Boolean)?;
                
                let then_type = self.infer_expr(then_branch)?;
                
                if let Some(else_branch) = else_branch {
                    let else_type = self.infer_expr(else_branch)?;
                    self.unify(&then_type, &else_type)?;
                } else {
                    // 如果没有 else 分支，则返回 Null
                    self.unify(&then_type, &Type::Null)?;
                }
                
                Ok(then_type)
            }
            Expr::Block(stmts, expr) => {
                // 创建新的类型环境
                let mut block_env = self.env.clone_with_non_generic();
                let mut block_checker = TypeChecker {
                    env: block_env,
                    subst: self.subst.clone(),
                };
                
                // 处理语句
                for stmt in stmts {
                    block_checker.infer_stmt(stmt)?;
                }
                
                // 处理最终表达式
                let result_type = if let Some(expr) = expr {
                    block_checker.infer_expr(expr)?
                } else {
                    Type::Null
                };
                
                // 更新替换
                self.subst = block_checker.subst;
                
                Ok(result_type)
            }
            Expr::Match(expr, cases) => {
                let expr_type = self.infer_expr(expr)?;
                
                // 所有分支必须返回相同类型
                let mut result_type = None;
                
                for case in cases {
                    // 创建新的环境用于模式匹配
                    let mut case_env = self.env.clone_with_non_generic();
                    let mut case_checker = TypeChecker {
                        env: case_env,
                        subst: self.subst.clone(),
                    };
                    
                    // 处理模式绑定
                    // 这里需要确保我们传递的是原始表达式的类型，而不是期望一个特定的构造器
                    case_checker.infer_pattern(&case.pattern, &expr_type)?;
                    
                    // 推导分支体类型
                    let case_type = case_checker.infer_expr(&case.body)?;
                    
                    // 更新替换
                    self.subst = case_checker.subst;
                    
                    // 统一所有分支类型
                    if let Some(rt) = &result_type {
                        self.unify(rt, &case_type)?;
                    } else {
                        result_type = Some(case_type);
                    }
                }
                
                result_type.ok_or_else(|| TypeError::InferenceFailure("空的 match 表达式".to_string()))
            }
            _ => Ok(Type::Any), // 暂时处理为 Any 类型
        }
    }

    // 推导字面量类型
    fn infer_literal(&mut self, lit: &Literal) -> TypeResult<Type> {
        match lit {
            Literal::Unit => Ok(Type::Null),
            Literal::Number(_) => Ok(Type::Number),
            Literal::String(_) => Ok(Type::String),
            Literal::Boolean(_) => Ok(Type::Boolean),
            Literal::Null => Ok(Type::Null),
            Literal::Array(elements) => {
                if elements.is_empty() {
                    // 空数组，创建类型变量
                    let elem_type = self.env.new_type_var();
                    Ok(Type::Array(Box::new(elem_type)))
                } else {
                    // 推导第一个元素类型
                    let first_type = self.infer_expr(&elements[0])?;
                    
                    // 确保所有元素类型一致
                    for elem in elements.iter().skip(1) {
                        let elem_type = self.infer_expr(elem)?;
                        self.unify(&first_type, &elem_type)?;
                    }
                    
                    Ok(Type::Array(Box::new(first_type)))
                }
            }
            Literal::Object(fields) => {
                let mut field_types = HashMap::new();
                
                for (name, value) in fields {
                    let value_type = self.infer_expr(value)?;
                    field_types.insert(name.clone(), value_type);
                }
                
                Ok(Type::Record(field_types))
            }
        }
    }

    // 推导模式类型
    // 推导模式类型
    fn infer_pattern(&mut self, pattern: &Pattern, value_type: &Type) -> TypeResult<()> {
            // 简单变量模式
            if pattern.params.is_none() {
                self.env.add_var(pattern.name.clone(), value_type.clone());
                return Ok(());
            }
            
            // 构造器模式
            if let Some(params) = &pattern.params {
                // 检查值类型是否为代数数据类型
                if let Type::Generic(type_name, _) = value_type {
                    // 获取构造器名称
                    let constructor_name = &pattern.name;
                    
                    // 查找构造器函数 - 先获取并克隆构造器类型，避免后续借用冲突
                    let constructor_type_opt = self.env.get_var(constructor_name).cloned();
                    
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
                                            self.env.add_var(name, param_type);
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
                                        self.env.add_var(param.name.clone(), param_type);
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
    
    // 推导语句类型
    pub fn infer_stmt(&mut self, stmt: &Stmt) -> TypeResult<()> {
        match stmt {
            Stmt::FunctionDecl(name, params, body) => {
                // 创建新的类型环境用于函数定义
                let mut fn_env = self.env.clone_with_non_generic();
                let mut fn_checker = TypeChecker {
                    env: fn_env,
                    subst: self.subst.clone(),
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
                self.env.add_var(name.clone(), fn_type);

                // 更新替换
                self.subst = fn_checker.subst;
                Ok(())
            }
            Stmt::VariableDecl(name, type_annotation, initializer) => {
                let var_type = self.infer_expr(initializer)?;
                
                // 如果有类型注解，检查它是否与推导类型匹配
                if let Some(annotation) = type_annotation {
                    let annotated_type = self.convert_type_annotation(annotation)?;
                    self.unify(&var_type, &annotated_type)?;
                }
                
                self.env.add_var(name.clone(), var_type);
                Ok(())
            }
            Stmt::TypeDecl(name, type_params, type_def) => {
                // 处理类型定义
                let ty = Type::Generic(name.clone(), vec![]);
                self.env.add_type(name.clone(), ty.clone());
                
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
                                        self.convert_type_annotation(annotation)?
                                    } else {
                                        Type::Any
                                    };
                                    param_types.push(param_type);
                                }
                            }
                            
                            // 创建构造器函数类型并添加到环境
                            let constructor_type = Type::Function(param_types, Box::new(ty.clone()));
                            self.env.add_var(constructor_name, constructor_type);
                        }
                    },
                    TypeDefinition::Record(fields) => {
                        // 对于记录类型，可能需要不同的处理方式
                        // 这里简单地注册类型名称，不创建构造器
                    }
                }
                
                Ok(())
            }
            Stmt::EffectDecl(name, _, _) => {
                // 简化处理效应定义
                Ok(())
            }
            Stmt::Expression(expr) => {
                self.infer_expr(expr)?;
                Ok(())
            }
        }
    }

    // 推导程序类型
    pub fn infer_program(&mut self, stmts: &[Stmt]) -> TypeResult<Type> {
        for stmt in stmts.iter().take(stmts.len() - 1) {
            self.infer_stmt(stmt)?;
        }
        
        // 最后一个语句的类型作为程序的类型
        if let Some(last) = stmts.last() {
            match last {
                Stmt::Expression(expr) => self.infer_expr(expr),
                _ => {
                    self.infer_stmt(last)?;
                    Ok(Type::Null)
                }
            }
        } else {
            Ok(Type::Null)
        }
    }

    // 实例化泛型类型
    pub fn instantiate_generic(&mut self, generic_type: &Type) -> Type {
            match generic_type {
                Type::Generic(name, type_args) => {
                    // 为每个类型参数创建新的类型变量
                    let mut subst = TypeSubst::new();
                    let mut new_args = Vec::new();
                    
                    for arg in type_args {
                        if let Type::Var(id) = arg {
                            let new_var = self.env.new_type_var();
                            subst.add(*id, new_var.clone());
                            new_args.push(new_var);
                        } else {
                            new_args.push(self.instantiate_generic(arg));
                        }
                    }
                    
                    Type::Generic(name.clone(), new_args)
                },
                Type::Function(params, ret) => {
                    let new_params = params.iter()
                        .map(|p| self.instantiate_generic(p))
                        .collect();
                    let new_ret = Box::new(self.instantiate_generic(ret));
                    Type::Function(new_params, new_ret)
                },
                Type::Array(elem) => {
                    Type::Array(Box::new(self.instantiate_generic(elem)))
                },
                Type::Record(fields) => {
                    let mut new_fields = HashMap::new();
                    for (name, field_type) in fields {
                        new_fields.insert(name.clone(), self.instantiate_generic(field_type));
                    }
                    Type::Record(new_fields)
                },
                // 其他类型直接返回
                _ => generic_type.clone(),
            }
        }
    
    
    // 辅助函数：检查字面量类型
    // Fix: Make this a method of TypeChecker instead of a standalone function
    pub fn check_literal(&self, lit: &Literal) -> TypeResult<Type> {
        match lit {
            Literal::Unit => Ok(Type::Null),
            Literal::Number(_) => Ok(Type::Number),
            Literal::String(_) => Ok(Type::String),
            Literal::Boolean(_) => Ok(Type::Boolean),
            Literal::Null => Ok(Type::Null),
            Literal::Array(elements) => {
                if elements.is_empty() {
                    // 空数组，无法确定类型，返回Any
                    return Ok(Type::Array(Box::new(Type::Any)));
                }
                
                // 检查第一个元素的类型
                // Fix: Use self.check_expr instead of check_expr
                let first_type = self.check_expr(&elements[0])?;
                
                // 检查所有其他元素是否与第一个元素类型相同
                for (i, element) in elements.iter().enumerate().skip(1) {
                    // Fix: Use self.check_expr instead of check_expr
                    let element_type = self.check_expr(element)?;
                    if element_type != first_type {
                        return Err(TypeError::TypeMismatch {
                            expected: first_type.to_string(),
                            actual: element_type.to_string(),
                        });
                    }
                }
                
                Ok(Type::Array(Box::new(first_type)))
            },
            Literal::Object(fields) => {
                let mut field_types = HashMap::new();
                for (name, value) in fields {
                    // Fix: Use self.check_expr instead of check_expr
                    let value_type = self.check_expr(value)?;
                    field_types.insert(name.clone(), value_type);
                }
                Ok(Type::Record(field_types))
            },
        }
    }
    
    // 辅助函数：检查表达式类型
    // Fix: Make this a method of TypeChecker instead of a standalone function
    pub fn check_expr(&self, expr: &Expr) -> TypeResult<Type> {
        match expr {
            // Fix: Use self.check_literal instead of check_literal
            Expr::Literal(lit) => self.check_literal(lit),
            // 其他表达式类型的检查可以在这里添加
            _ => Ok(Type::Any), // 暂时对其他表达式返回Any类型
        }
    }

    // 初始化标准库函数
    pub fn init_stdlib(&mut self, stdlib_env: &Rc<RefCell<Environment>>) {
        // 从标准库环境中获取所有函数名和值
        let env = stdlib_env.borrow();
        
        // 直接访问环境中的值
        // 注意：这需要Environment结构有一个可以访问的方法或字段
        // 这里假设Environment有一个values字段或get_values方法
        for (name, value) in env.values.iter() {
            // 根据函数的参数和返回类型创建函数类型
            match value {
                Value::NativeFunction(native_fn) => {
                    // 为每个参数创建类型变量
                    let param_types: Vec<Type> = native_fn.params.iter()
                        .map(|_| Type::Any)
                        .collect();
                    
                    // 创建函数类型并添加到环境
                    let fn_type = Type::Function(param_types, Box::new(Type::Any));
                    self.env.add_var(name.clone(), fn_type);
                },
                Value::Function(func) => {
                    // 为用户定义的函数创建类型
                    let param_types: Vec<Type> = func.params.iter()
                        .map(|_| Type::Any)
                        .collect();
                    
                    let fn_type = Type::Function(param_types, Box::new(Type::Any));
                    self.env.add_var(name.clone(), fn_type);
                },
                // 对于非函数值，添加具体类型
                Value::Number(_) => self.env.add_var(name.clone(), Type::Number),
                Value::String(_) => self.env.add_var(name.clone(), Type::String),
                Value::Boolean(_) => self.env.add_var(name.clone(), Type::Boolean),
                Value::Array(items) => {
                    let elem_type = if items.is_empty() {
                        Type::Any
                    } else {
                        // 从第一个元素推断类型
                        match &items[0] {
                            Value::Number(_) => Type::Number,
                            Value::String(_) => Type::String,
                            Value::Boolean(_) => Type::Boolean,
                            _ => Type::Any,
                        }
                    };
                    self.env.add_var(name.clone(), Type::Array(Box::new(elem_type)));
                },
                _ => self.env.add_var(name.clone(), Type::Any),
            }
        }
    }
}
