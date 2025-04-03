use std::collections::{HashMap, HashSet};
use std::fmt;
use std::rc::Rc;
use std::cell::RefCell;
use thiserror::Error;

use crate::ast::{Expr, Literal, TypeAnnotation, BinaryOp, UnaryOp, Stmt, Parameter, Pattern, MatchCase};
use crate::runtime::Value;

#[derive(Error, Debug)]
pub enum TypeError {
    #[error("类型不匹配: 期望 {expected}, 得到 {actual}")]
    TypeMismatch {
        expected: String,
        actual: String,
    },
    #[error("无法推导类型: {0}")]
    InferenceFailure(String),
    #[error("未定义的变量: {0}")]
    UndefinedVariable(String),
    #[error("递归类型定义")]
    RecursiveType,
    #[error("类型变量逃逸")]
    TypeVariableEscape,
    #[error("未知类型: {0}")]
    UnknownType(String),
    #[error("类型不统一: {0} 和 {1}")]
    UnificationFailure(String, String),
}

pub type TypeResult<T> = Result<T, TypeError>;

// 类型变量 ID
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct TypeVarId(pub usize);


// 类型表达式
#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    Number,
    String,
    Boolean,
    Array(Box<Type>),
    Function(Vec<Type>, Box<Type>),
    Record(HashMap<String, Type>),
    Var(TypeVarId),
    Generic(String, Vec<Type>),
    Any,
    Null,
}



impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Type::Number => write!(f, "Number"),
            Type::String => write!(f, "String"),
            Type::Boolean => write!(f, "Boolean"),
            Type::Array(elem_type) => write!(f, "[{}]", elem_type),
            Type::Function(param_types, return_type) => {
                write!(f, "(")?;
                for (i, param) in param_types.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}", param)?;
                }
                write!(f, ") => {}", return_type)
            }
            Type::Record(fields) => {
                write!(f, "{{ ")?;
                for (i, (name, field_type)) in fields.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}: {}", name, field_type)?;
                }
                write!(f, " }}")
            }
            Type::Var(id) => write!(f, "t{}", id.0),
            Type::Generic(name, args) => {
                if args.is_empty() {
                    write!(f, "{}", name)
                } else {
                    write!(f, "{}<", name)?;
                    for (i, arg) in args.iter().enumerate() {
                        if i > 0 {
                            write!(f, ", ")?;
                        }
                        write!(f, "{}", arg)?;
                    }
                    write!(f, ">")
                }
            }
            Type::Any => write!(f, "Any"),
            Type::Null => write!(f, "Null"),
        }
    }
}


// 类型环境
#[derive(Debug, Clone)]
pub struct TypeEnv {
    // 变量类型映射
    vars: HashMap<String, Type>,
    // 类型定义映射
    types: HashMap<String, Type>,
    // 非泛型变量集合
    non_generic: HashSet<TypeVarId>,
    // 下一个可用的类型变量 ID
    next_var_id: usize,
}


impl TypeEnv {
    pub fn new() -> Self {
        TypeEnv {
            vars: HashMap::new(),
            types: HashMap::new(),
            non_generic: HashSet::new(),
            next_var_id: 0,
        }
    }

    // 创建新的类型变量
    pub fn new_type_var(&mut self) -> Type {
        let id = TypeVarId(self.next_var_id);
        self.next_var_id += 1;
        Type::Var(id)
    }

    // 添加变量类型
    pub fn add_var(&mut self, name: String, ty: Type) {
        self.vars.insert(name, ty);
    }

    // 获取变量类型
    pub fn get_var(&self, name: &str) -> Option<&Type> {
        self.vars.get(name)
    }

    // 添加类型定义
    pub fn add_type(&mut self, name: String, ty: Type) {
        self.types.insert(name, ty);
    }

    // 获取类型定义
    pub fn get_type(&self, name: &str) -> Option<&Type> {
        self.types.get(name)
    }

    // 标记类型变量为非泛型
    pub fn mark_non_generic(&mut self, var_id: TypeVarId) {
        self.non_generic.insert(var_id);
    }

    // 检查类型变量是否为非泛型
    pub fn is_non_generic(&self, var_id: TypeVarId) -> bool {
        self.non_generic.contains(&var_id)
    }

    // 创建环境的副本
    pub fn clone_with_non_generic(&self) -> Self {
        TypeEnv {
            vars: self.vars.clone(),
            types: self.types.clone(),
            non_generic: self.non_generic.clone(),
            next_var_id: self.next_var_id,
        }
    }
}


// 类型替换
#[derive(Debug, Clone)]
pub struct TypeSubst(HashMap<TypeVarId, Type>);

impl TypeSubst {
    pub fn new() -> Self {
        TypeSubst(HashMap::new())
    }

    // 添加替换
    pub fn add(&mut self, var_id: TypeVarId, ty: Type) {
        self.0.insert(var_id, ty);
    }

    // 应用替换到类型
    pub fn apply(&self, ty: &Type) -> Type {
        match ty {
            Type::Var(id) => {
                if let Some(subst_ty) = self.0.get(id) {
                    self.apply(subst_ty)
                } else {
                    ty.clone()
                }
            }
            Type::Array(elem_ty) => {
                Type::Array(Box::new(self.apply(elem_ty)))
            }
            Type::Function(param_tys, return_ty) => {
                let new_params = param_tys.iter().map(|p| self.apply(p)).collect();
                let new_return = Box::new(self.apply(return_ty));
                Type::Function(new_params, new_return)
            }
            Type::Record(fields) => {
                let mut new_fields = HashMap::new();
                for (name, field_ty) in fields {
                    new_fields.insert(name.clone(), self.apply(field_ty));
                }
                Type::Record(new_fields)
            }
            Type::Generic(name, args) => {
                let new_args = args.iter().map(|a| self.apply(a)).collect();
                Type::Generic(name.clone(), new_args)
            }
            _ => ty.clone(),
        }
    }

    // 组合两个替换
    pub fn compose(&self, other: &TypeSubst) -> TypeSubst {
        let mut result = self.clone();
        
        for (id, ty) in &other.0 {
            let new_ty = self.apply(ty);
            result.0.insert(*id, new_ty);
        }
        
        result
    }
}

// 类型检查器
pub struct TypeChecker {
    env: TypeEnv,
    subst: TypeSubst,
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
                    // 算术运算符
                    BinaryOp::Add | BinaryOp::Subtract | BinaryOp::Multiply | 
                    BinaryOp::Divide | BinaryOp::Modulo => {
                        self.unify(&left_type, &Type::Number)?;
                        self.unify(&right_type, &Type::Number)?;
                        Ok(Type::Number)
                    }
                    // 字符串连接
                    BinaryOp::Add => {
                        self.unify(&left_type, &Type::String)?;
                        self.unify(&right_type, &Type::String)?;
                        Ok(Type::String)
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
                
                // 为每个参数创建类型变量
                let mut param_types = Vec::new();
                for arg in args {
                    let arg_type = self.infer_expr(arg)?;
                    param_types.push(arg_type);
                }
                
                // 创建返回类型变量
                let return_type = self.env.new_type_var();
                
                // 构造函数类型并统一
                let fn_type = Type::Function(param_types, Box::new(return_type.clone()));
                self.unify(&callee_type, &fn_type)?;
                
                Ok(return_type)
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
            // 其他表达式类型...
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
    fn infer_pattern(&mut self, pattern: &Pattern, value_type: &Type) -> TypeResult<()> {
        // 简单变量模式
        if pattern.params.is_none() {
            self.env.add_var(pattern.name.clone(), value_type.clone());
            return Ok(());
        }
        
        // 构造器模式
        if let Some(params) = &pattern.params {
            // 假设值类型是记录类型
            if let Type::Record(fields) = value_type {
                // 检查构造器名称
                if let Some(constructor) = fields.get("constructor") {
                    if let Type::String = constructor {
                        // 假设参数存储在 args 字段中
                        if let Some(args) = fields.get("args") {
                            if let Type::Array(elem_type) = args {
                                // 为每个模式参数绑定类型
                                for (i, param) in params.iter().enumerate() {
                                    let param_type = self.env.new_type_var();
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
            Stmt::VariableDecl(name, initializer) => {
                let var_type = self.infer_expr(initializer)?;
                self.env.add_var(name.clone(), var_type);
                Ok(())
            }
            Stmt::TypeDecl(name, type_params, type_def) => {
                // 处理类型定义
                // 这里简化处理，实际应该解析类型定义
                let ty = Type::Generic(name.clone(), vec![]);
                self.env.add_type(name.clone(), ty);
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
}


pub fn check_literal(lit: &Literal) -> TypeResult<Type> {
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
            let first_type = check_expr(&elements[0])?;
            
            // 检查所有其他元素是否与第一个元素类型相同
            for (i, element) in elements.iter().enumerate().skip(1) {
                let element_type = check_expr(element)?;
                if element_type != first_type {
                    return Err(TypeError::TypeMismatch {
                        expected: first_type.to_string(),
                        actual: element_type.to_string(),
                    });
                }
            }
            
            Ok(Type::Array(Box::new(first_type)))
        },
        Literal::Object(_) => Ok(Type::Any), // 暂时对对象返回Any类型
    }
}



pub fn check_expr(expr: &Expr) -> TypeResult<Type> {
    match expr {
        Expr::Literal(lit) => check_literal(lit),
        // 其他表达式类型的检查可以在这里添加
        _ => Ok(Type::Any), // 暂时对其他表达式返回Any类型
    }
}
