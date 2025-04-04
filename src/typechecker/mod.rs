// 导出子模块
mod types;
mod env;
mod subst;
mod error;
mod checker;

// 重新导出主要类型和函数
pub use types::{Type, TypeVarId};
pub use env::TypeEnv;
pub use subst::TypeSubst;
pub use error::{TypeError, TypeResult};
pub use checker::TypeChecker;

// 提供一个简单的类型检查入口函数
use crate::ast::{Stmt, Literal, Expr};

pub fn typecheck(stmts: &[Stmt]) -> TypeResult<Type> {
    let mut checker = TypeChecker::new();
    checker.init_builtins();
    checker.infer_program(stmts)
}

// 导出辅助函数，用于外部模块调用
pub fn check_literal(lit: &Literal) -> TypeResult<Type> {
    let checker = TypeChecker::new();
    checker.check_literal(lit)
}

pub fn check_expr(expr: &Expr) -> TypeResult<Type> {
    let checker = TypeChecker::new();
    checker.check_expr(expr)
}