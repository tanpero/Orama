pub mod types;
pub mod env;
pub mod subst;
pub mod error;
pub mod checker;
pub mod expr;
pub mod stmt;
pub mod pattern;
pub mod literal;
pub mod unify;

pub use checker::TypeChecker;
use crate::ast::{Literal, Expr, Stmt};
use error::TypeResult;
use types::Type;

// Export these functions for external use
pub fn typecheck(stmts: &[Stmt]) -> TypeResult<Type> {
    let mut checker = TypeChecker::new();
    checker.init_builtins();
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
