pub mod checker;
pub mod env;
pub mod error;
pub mod expr;
pub mod literal;
pub mod pattern;
pub mod stmt;
pub mod subst;
pub mod types;
pub mod unify;

use crate::ast::{Expr, Literal, Stmt};
pub use checker::TypeChecker;
use error::TypeResult;
use types::Type;

// Export these functions for external use
pub fn typecheck(stmts: &[Stmt]) -> TypeResult<Type> {
    let mut checker = TypeChecker::new();
    checker.init_builtins();

    // 第一遍：注册所有函数声明的类型
    for stmt in stmts {
        if let Stmt::VariableDecl(name, _, expr) = stmt {
            // 检查是否是函数定义
            if let Expr::Function(params, _) = expr {
                // 为每个参数创建类型变量
                let mut param_types = Vec::new();
                for _ in params {
                    param_types.push(Type::Any); // 暂时使用Any类型
                }

                // 创建函数类型并添加到环境
                let fn_type = Type::Function(param_types, Box::new(Type::Any));
                checker.env.add_var(name.clone(), fn_type);
            }
        }
    }

    // 第二遍：实际检查所有语句
    checker.infer_program(stmts)
}

// Add this function to fix the error in expr_parser.rs
pub fn check_literal(lit: &Literal) -> TypeResult<Type> {
    let checker = TypeChecker::new();
    checker.check_literal(lit)
}

pub fn check_expr(expr: &Expr) -> TypeResult<Type> {
    let checker = TypeChecker::new();
    checker.check_expr(expr)
}
