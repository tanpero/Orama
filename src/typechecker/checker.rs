use crate::ast::TypeDefinition;
use crate::ast::{
    BinaryOp, Expr, Literal, MatchCase, Parameter, Pattern, Stmt, TypeAnnotation, UnaryOp,
};
use crate::runtime::Environment;
use crate::runtime::Value;
use crate::typechecker::env::TypeEnv;
use crate::typechecker::error::{TypeError, TypeResult};
use crate::typechecker::subst::TypeSubst;
use crate::typechecker::types::{Type, TypeVarId};
use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

// 导入拆分出去的模块
use crate::typechecker::expr::ExprTypeChecker;
use crate::typechecker::literal::LiteralTypeChecker;
use crate::typechecker::literal::MutableLiteralTypeChecker;
use crate::typechecker::pattern::PatternTypeChecker;
use crate::typechecker::stmt::StmtTypeChecker;
use crate::typechecker::unify::UnifyTypeChecker;

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

        // 注册 List 类型
        let list_type_var = self.env.new_type_var();
        self.env.add_type(
            "List".to_string(),
            Type::Generic("List".to_string(), vec![list_type_var]),
        );
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
                let param_types = fn_type
                    .params
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

    // 推导表达式类型 - 委托给 ExprTypeChecker
    pub fn infer_expr(&mut self, expr: &Expr) -> TypeResult<Type> {
        let mut expr_checker = ExprTypeChecker::new(self);
        expr_checker.infer_expr(expr)
    }

    // 推导语句类型 - 委托给 StmtTypeChecker
    pub fn infer_stmt(&mut self, stmt: &Stmt) -> TypeResult<()> {
        match stmt {
            Stmt::FunctionDecl(name, params, body) => {
                // 创建新的类型环境用于函数定义
                let mut fn_env = self.env.clone_with_non_generic();

                // 处理函数参数
                let mut param_types = Vec::new();
                for param in params {
                    let param_type = if let Some(annotation) = &param.type_annotation {
                        self.convert_type_annotation(annotation)?
                    } else {
                        self.env.new_type_var()
                    };
                    param_types.push(param_type.clone());
                }

                // 创建返回类型变量
                let return_type = self.env.new_type_var();

                // 构造函数类型并先添加到环境中，以支持递归
                let fn_type = Type::Function(param_types.clone(), Box::new(return_type.clone()));
                self.env.add_var(name.clone(), fn_type.clone());

                // 现在创建函数检查器并添加参数
                let mut fn_checker = TypeChecker {
                    env: fn_env,
                    subst: self.subst.clone(),
                };

                // 将函数自身添加到函数环境中，以支持递归
                fn_checker.env.add_var(name.clone(), fn_type);

                // 添加参数到函数环境
                for (param, param_type) in params.iter().zip(param_types.iter()) {
                    fn_checker
                        .env
                        .add_var(param.name.clone(), param_type.clone());
                }

                // 推导函数体类型
                let body_type = fn_checker.infer_expr(body)?;

                // 统一函数体类型与返回类型
                fn_checker.unify(&return_type, &body_type)?;

                // 更新替换
                self.subst = fn_checker.subst;
                Ok(())
            }
            Stmt::VariableDecl(name, type_annotation, initializer) => {
                // 特殊处理函数表达式
                if let Expr::Function(params, body) = initializer {
                    // 创建新的类型环境
                    let mut fn_env = self.env.clone_with_non_generic();

                    // 处理函数参数
                    let mut param_types = Vec::new();
                    for param in params {
                        let param_type = if let Some(annotation) = &param.type_annotation {
                            self.convert_type_annotation(annotation)?
                        } else {
                            fn_env.new_type_var()
                        };
                        param_types.push(param_type.clone());
                    }

                    // 创建返回类型变量
                    let return_type = fn_env.new_type_var();

                    // 构造函数类型
                    let fn_type =
                        Type::Function(param_types.clone(), Box::new(return_type.clone()));

                    // 先将函数添加到环境中，以支持递归
                    self.env.add_var(name.clone(), fn_type.clone());

                    // 创建函数检查器
                    let mut fn_checker = TypeChecker {
                        env: fn_env,
                        subst: self.subst.clone(),
                    };

                    // 将函数自身添加到函数环境中，以支持递归
                    fn_checker.env.add_var(name.clone(), fn_type.clone());

                    // 添加参数到函数环境
                    for (param, param_type) in params.iter().zip(param_types.iter()) {
                        fn_checker
                            .env
                            .add_var(param.name.clone(), param_type.clone());
                    }

                    // 推导函数体类型
                    let body_type = fn_checker.infer_expr(body)?;

                    // 统一返回类型
                    fn_checker.unify(&return_type, &body_type)?;

                    // 更新替换
                    self.subst = fn_checker.subst;

                    return Ok(());
                }

                // 常规变量声明处理
                let var_type = self.infer_expr(initializer)?;

                // 如果有类型注解，检查它是否与推导类型匹配
                if let Some(annotation) = type_annotation {
                    let annotated_type = self.convert_type_annotation(annotation)?;
                    self.unify(&var_type, &annotated_type)?;
                }

                self.env.add_var(name.clone(), var_type);
                Ok(())
            }
            Stmt::EffectDecl(_, _, _) => {
                let mut stmt_checker = StmtTypeChecker::new(self);
                stmt_checker.infer_stmt(stmt)
            }
            Stmt::TypeDecl(_, _, _) => {
                let mut stmt_checker = StmtTypeChecker::new(self);
                stmt_checker.infer_stmt(stmt)
            }
            Stmt::Expression(_) => {
                let mut stmt_checker = StmtTypeChecker::new(self);
                stmt_checker.infer_stmt(stmt)
            }
            Stmt::FunctionDecl(name, params, body) => {
                // 创建新的类型环境用于函数定义
                let mut fn_env = self.env.clone_with_non_generic();

                // 处理函数参数
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

                // 创建返回类型变量
                let return_type_var = fn_env.new_type_var();

                // 构造函数类型并提前添加到环境中，以支持递归
                let fn_type =
                    Type::Function(param_types.clone(), Box::new(return_type_var.clone()));
                self.env.add_var(name.clone(), fn_type.clone());
                fn_env.add_var(name.clone(), fn_type);

                // 推导函数体类型
                let mut fn_checker = TypeChecker {
                    env: fn_env,
                    subst: self.subst.clone(),
                };
                let body_type = fn_checker.infer_expr(body)?;

                // 统一返回类型
                fn_checker.unify(&return_type_var, &body_type)?;

                // 更新替换
                self.subst = fn_checker.subst;

                // 更新环境中的函数类型
                let final_return_type = self.subst.apply(&body_type);
                let final_fn_type = Type::Function(param_types, Box::new(final_return_type));
                self.env.add_var(name.clone(), final_fn_type);

                Ok(())
            } // ... 其他语句类型的处理保持不变 ...
        }
    }

    // 推导模式类型 - 委托给 PatternTypeChecker
    pub fn infer_pattern(&mut self, pattern: &Pattern, value_type: &Type) -> TypeResult<()> {
        let mut pattern_checker = PatternTypeChecker::new(self);
        pattern_checker.infer_pattern(pattern, value_type)
    }

    // 推导字面量类型 - 委托给 LiteralTypeChecker
    // Infer literal type - delegate to MutableLiteralTypeChecker
    pub fn infer_literal(&mut self, lit: &Literal) -> TypeResult<Type> {
        let mut literal_checker = MutableLiteralTypeChecker::new(self);
        literal_checker.infer_literal(lit)
    }

    // 统一两个类型 - 委托给 UnifyTypeChecker
    pub fn unify(&mut self, t1: &Type, t2: &Type) -> TypeResult<()> {
        let mut unify_checker = UnifyTypeChecker::new(self);
        unify_checker.unify(t1, t2)
    }

    // 统一类型变量与类型 - 委托给 UnifyTypeChecker
    pub fn unify_var(&mut self, var_id: TypeVarId, ty: Type) -> TypeResult<()> {
        let mut unify_checker = UnifyTypeChecker::new(self);
        unify_checker.unify_var(var_id, ty)
    }

    // 检查函数调用与函数签名是否匹配 - 委托给 ExprTypeChecker
    pub fn check_function_call(&mut self, fn_type: &Type, args: &[Expr]) -> TypeResult<Type> {
        let mut expr_checker = ExprTypeChecker::new(self);
        expr_checker.check_function_call(fn_type, args)
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
            }
            Type::Function(params, ret) => {
                let new_params = params.iter().map(|p| self.instantiate_generic(p)).collect();
                let new_ret = Box::new(self.instantiate_generic(ret));
                Type::Function(new_params, new_ret)
            }
            Type::Array(elem) => Type::Array(Box::new(self.instantiate_generic(elem))),
            Type::Record(fields) => {
                let mut new_fields = HashMap::new();
                for (name, field_type) in fields {
                    new_fields.insert(name.clone(), self.instantiate_generic(field_type));
                }
                Type::Record(new_fields)
            }
            // 其他类型直接返回
            _ => generic_type.clone(),
        }
    }

    // 辅助函数：检查字面量类型
    pub fn check_literal(&self, lit: &Literal) -> TypeResult<Type> {
        let literal_checker = LiteralTypeChecker::new(self);
        literal_checker.check_literal(lit)
    }

    // 辅助函数：检查表达式类型
    pub fn check_expr(&self, expr: &Expr) -> TypeResult<Type> {
        match expr {
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
        for (name, value) in env.values.iter() {
            // 根据函数的参数和返回类型创建函数类型
            match value {
                Value::NativeFunction(native_fn) => {
                    // 为特定的标准库函数指定正确的返回类型
                    let return_type = match name.as_str() {
                        "to_number" => Type::Number,
                        "to_string" => Type::String,
                        "input" => Type::String,
                        "random" => Type::Number,
                        "floor" => Type::Number,
                        "ceil" => Type::Number,
                        "round" => Type::Number,
                        "println" | "print" => Type::Null,
                        "length" => Type::Number,
                        _ => Type::Any,
                    };

                    // 为每个参数创建类型变量
                    let param_types: Vec<Type> =
                        native_fn.params.iter().map(|_| Type::Any).collect();

                    // 创建函数类型并添加到环境
                    let fn_type = Type::Function(param_types, Box::new(return_type));
                    self.env.add_var(name.clone(), fn_type);
                }
                Value::Function(func) => {
                    // 为用户定义的函数创建类型
                    let param_types: Vec<Type> = func.params.iter().map(|_| Type::Any).collect();

                    let fn_type = Type::Function(param_types, Box::new(Type::Any));
                    self.env.add_var(name.clone(), fn_type);
                }
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
                    self.env
                        .add_var(name.clone(), Type::Array(Box::new(elem_type)));
                }
                _ => self.env.add_var(name.clone(), Type::Any),
            }
        }
    }
    // 获取下一个变量ID
    pub fn get_next_var_id(&self) -> usize {
        self.env.next_var_id
    }
}
