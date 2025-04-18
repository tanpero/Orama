use crate::ast::{BinaryOp, Expr, UnaryOp};
use crate::typechecker::checker::TypeChecker;
use crate::typechecker::error::{TypeError, TypeResult};
use crate::typechecker::types::Type;

pub struct ExprTypeChecker<'a> {
    checker: &'a mut TypeChecker,
}

impl<'a> ExprTypeChecker<'a> {
    pub fn new(checker: &'a mut TypeChecker) -> Self {
        ExprTypeChecker { checker }
    }

    // 推导表达式类型
    pub fn infer_expr(&mut self, expr: &Expr) -> TypeResult<Type> {
        match expr {
            Expr::Literal(lit) => self.checker.infer_literal(lit),
            Expr::Variable(name) => {
                if let Some(ty) = self.checker.env.get_var(name) {
                    Ok(ty.clone())
                } else {
                    Err(TypeError::UndefinedVariable(name.clone()))
                }
            }
            Expr::Binary(left, op, right) => self.infer_binary_expr(left, op, right),
            Expr::Unary(op, expr) => self.infer_unary_expr(op, expr),
            Expr::Call(callee, args) => {
                let callee_type = self.infer_expr(callee)?;
                self.checker.check_function_call(&callee_type, args)
            }
            Expr::Function(params, body) => self.infer_function_expr(params, body),
            Expr::If(condition, then_branch, else_branch) => {
                self.infer_if_expr(condition, then_branch, else_branch)
            }
            Expr::Block(stmts, expr) => self.infer_block_expr(stmts, expr),
            Expr::Match(expr, cases) => self.infer_match_expr(expr, cases),
            _ => Ok(Type::Any), // 暂时处理为 Any 类型
        }
    }

    // 推导二元表达式类型
    // Fix the dereferencing issues in the binary expression handler
    fn infer_binary_expr(&mut self, left: &Expr, op: &BinaryOp, right: &Expr) -> TypeResult<Type> {
        let left_type = self.infer_expr(left)?;
        let right_type = self.infer_expr(right)?;

        match op {
            BinaryOp::Index => {
                // 检查右侧是否为字符串（对象键）
                self.checker.unify(&right_type, &Type::String)?;

                // 对于对象类型，我们需要检查是否为记录类型
                if let Type::Record(fields) = &left_type {
                    // 如果是字符串字面量，我们可以直接检查字段
                    if let Expr::Literal(crate::ast::Literal::String(field_name)) = right {
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
                        self.checker.unify(&right_type, &Type::Number)?;
                        return Ok(*elem_type.clone());
                    }

                    // 如果既不是记录也不是数组，报错
                    Err(TypeError::TypeMismatch {
                        expected: "对象或数组".to_string(),
                        actual: format!("{}", left_type),
                    })
                }
            }
            // 访问运算符
            BinaryOp::Access => {
                // 确保左侧是记录类型
                if let Type::Record(fields) = &left_type {
                    // 右侧必须是字符串字面量
                    if let Expr::Literal(crate::ast::Literal::String(field_name)) = right {
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

                // 处理类型变量的情况
                match (&left_type, &right_type) {
                    // 如果两边都是类型变量，默认推断为数字类型
                    (Type::Var(_), Type::Var(_)) => {
                        self.checker.unify(&left_type, &Type::Number)?;
                        self.checker.unify(&right_type, &Type::Number)?;
                        return Ok(Type::Number);
                    }
                    // 如果左边是类型变量，右边有具体类型
                    (Type::Var(_), _) => {
                        self.checker.unify(&left_type, &right_type)?;
                        return Ok(right_type);
                    }
                    // 如果右边是类型变量，左边有具体类型
                    (_, Type::Var(_)) => {
                        self.checker.unify(&right_type, &left_type)?;
                        return Ok(left_type);
                    }
                    // 处理 Any 类型的情况
                    (Type::Any, Type::Any) => return Ok(Type::Number),
                    (Type::Any, _) => return Ok(right_type),
                    (_, Type::Any) => return Ok(left_type),
                    // 其他情况尝试统一类型
                    _ => {
                        self.checker.unify(&left_type, &right_type)?;
                        let unified_type = self.checker.subst.apply(&left_type);
                        match unified_type {
                            Type::Number => Ok(Type::Number),
                            Type::String => Ok(Type::String),
                            _ => Err(TypeError::TypeMismatch {
                                expected: "Number 或 String".to_string(),
                                actual: format!("{}", unified_type),
                            }),
                        }
                    }
                }
            }
            // 算术运算符
            BinaryOp::Subtract | BinaryOp::Multiply | BinaryOp::Divide | BinaryOp::Modulo => {
                self.checker.unify(&left_type, &Type::Number)?;
                self.checker.unify(&right_type, &Type::Number)?;
                Ok(Type::Number)
            }
            // 比较运算符
            // 相等和不等操作符
            BinaryOp::Equal | BinaryOp::NotEqual => {
                // 如果两边都是 Number 类型，直接返回 Boolean
                if let (Type::Number, Type::Number) = (&left_type, &right_type) {
                    return Ok(Type::Boolean);
                }

                // 如果两边都是 String 类型，直接返回 Boolean
                if let (Type::String, Type::String) = (&left_type, &right_type) {
                    return Ok(Type::Boolean);
                }

                // 如果两边都是 Boolean 类型，直接返回 Boolean
                if let (Type::Boolean, Type::Boolean) = (&left_type, &right_type) {
                    return Ok(Type::Boolean);
                }

                // 尝试统一类型
                self.checker.unify(&left_type, &right_type)?;
                Ok(Type::Boolean)
            }

            // 比较操作符
            BinaryOp::Less | BinaryOp::LessEqual | BinaryOp::Greater | BinaryOp::GreaterEqual => {
                // 数字比较
                if let (Type::Number, Type::Number) = (&left_type, &right_type) {
                    return Ok(Type::Boolean);
                }

                // 字符串比较
                if let (Type::String, Type::String) = (&left_type, &right_type) {
                    return Ok(Type::Boolean);
                }

                // 尝试统一类型
                self.checker.unify(&left_type, &right_type)?;

                // 确保统一后的类型是可比较的
                let unified_type = self.checker.subst.apply(&left_type);
                match unified_type {
                    Type::Number | Type::String => Ok(Type::Boolean),
                    _ => Err(TypeError::TypeMismatch {
                        expected: "可比较类型 (Number 或 String)".to_string(),
                        actual: format!("{}", unified_type),
                    }),
                }
            }
            // 逻辑运算符
            BinaryOp::And | BinaryOp::Or => {
                self.checker.unify(&left_type, &Type::Boolean)?;
                self.checker.unify(&right_type, &Type::Boolean)?;
                Ok(Type::Boolean)
            }
        }
    }

    // 推导一元表达式类型
    fn infer_unary_expr(&mut self, op: &UnaryOp, expr: &Expr) -> TypeResult<Type> {
        let expr_type = self.infer_expr(expr)?;

        match op {
            UnaryOp::Negate => {
                self.checker.unify(&expr_type, &Type::Number)?;
                Ok(Type::Number)
            }
            UnaryOp::Not => {
                self.checker.unify(&expr_type, &Type::Boolean)?;
                Ok(Type::Boolean)
            }
        }
    }

    // 推导函数表达式类型
    fn infer_function_expr(
        &mut self,
        params: &[crate::ast::Parameter],
        body: &Expr,
    ) -> TypeResult<Type> {
        // 创建新的类型环境
        let mut fn_env = self.checker.env.clone_with_non_generic();

        // 为每个参数创建类型变量
        let mut param_types = Vec::new();
        for param in params {
            let param_type = if let Some(annotation) = &param.type_annotation {
                self.checker.convert_type_annotation(annotation)?
            } else {
                fn_env.new_type_var()
            };

            param_types.push(param_type.clone());
        }

        // 创建返回类型变量
        let return_type = fn_env.new_type_var();

        // 创建函数类型
        let fn_type = Type::Function(param_types.clone(), Box::new(return_type.clone()));

        // 为匿名函数创建一个唯一的名称，以支持递归
        // 使用一个特殊前缀，避免与用户定义的变量冲突
        let anonymous_fn_name = format!("__anonymous_fn_{}", self.checker.get_next_var_id());

        // 推导函数体类型
        let mut fn_checker = TypeChecker {
            env: fn_env,
            subst: self.checker.subst.clone(),
        };

        // 将匿名函数添加到环境中，以支持递归
        fn_checker.env.add_var(anonymous_fn_name, fn_type.clone());

        // 添加参数到环境
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
        self.checker.subst = fn_checker.subst;

        // 返回函数类型
        Ok(fn_type)
    }

    // 辅助方法：获取父级变量名（如果在变量赋值上下文中）
    fn get_parent_variable_name(&self) -> Option<&String> {
        // 这个方法需要访问AST上下文，这里是一个简化实现
        // 实际实现可能需要修改AST遍历逻辑或传递额外上下文
        None
    }

    // 推导 if 表达式类型
    fn infer_if_expr(
        &mut self,
        condition: &Expr,
        then_branch: &Expr,
        else_branch: &Option<Box<Expr>>,
    ) -> TypeResult<Type> {
        let cond_type = self.infer_expr(condition)?;
        self.checker.unify(&cond_type, &Type::Boolean)?;

        let then_type = self.infer_expr(then_branch)?;

        if let Some(else_branch) = else_branch {
            let else_type = self.infer_expr(else_branch)?;
            self.checker.unify(&then_type, &else_type)?;
        } else {
            // 如果没有 else 分支，则返回 Null
            self.checker.unify(&then_type, &Type::Null)?;
        }

        Ok(then_type)
    }

    // 推导块表达式类型
    fn infer_block_expr(
        &mut self,
        stmts: &[crate::ast::Stmt],
        expr: &Option<Box<Expr>>,
    ) -> TypeResult<Type> {
        // 创建新的类型环境
        let mut block_env = self.checker.env.clone_with_non_generic();
        let mut block_checker = TypeChecker {
            env: block_env,
            subst: self.checker.subst.clone(),
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
        self.checker.subst = block_checker.subst;

        Ok(result_type)
    }

    // 推导 match 表达式类型
    fn infer_match_expr(
        &mut self,
        expr: &Expr,
        cases: &[crate::ast::MatchCase],
    ) -> TypeResult<Type> {
        let expr_type = self.infer_expr(expr)?;

        // 所有分支必须返回相同类型
        let mut result_type = None;

        for case in cases {
            // 创建新的环境用于模式匹配
            let mut case_env = self.checker.env.clone_with_non_generic();
            let mut case_checker = TypeChecker {
                env: case_env,
                subst: self.checker.subst.clone(),
            };

            // 处理模式绑定
            case_checker.infer_pattern(&case.pattern, &expr_type)?;

            // 推导分支体类型
            let case_type = case_checker.infer_expr(&case.body)?;

            // 更新替换
            self.checker.subst = case_checker.subst;

            // 统一所有分支类型
            if let Some(rt) = &result_type {
                self.checker.unify(rt, &case_type)?;
            } else {
                result_type = Some(case_type);
            }
        }

        result_type.ok_or_else(|| TypeError::InferenceFailure("空的 match 表达式".to_string()))
    }

    // 检查函数调用与函数签名是否匹配
    pub fn check_function_call(&mut self, fn_type: &Type, args: &[Expr]) -> TypeResult<Type> {
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
                    if let Err(e) = self.checker.unify(&arg_type, expected_type) {
                        return Err(TypeError::TypeMismatch {
                            expected: format!("参数 #{}: {}", i + 1, expected_type),
                            actual: format!("参数 #{}: {}", i + 1, arg_type),
                        });
                    }
                }

                Ok(self.checker.subst.apply(return_type))
            }
            _ => Err(TypeError::TypeMismatch {
                expected: "函数类型".to_string(),
                actual: format!("{}", fn_type),
            }),
        }
    }
}
