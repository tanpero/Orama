use crate::typechecker::checker::TypeChecker;
use crate::typechecker::error::{TypeError, TypeResult};
use crate::typechecker::types::{Type, TypeVarId};

pub struct UnifyTypeChecker<'a> {
    checker: &'a mut TypeChecker,
}

impl<'a> UnifyTypeChecker<'a> {
    pub fn new(checker: &'a mut TypeChecker) -> Self {
        UnifyTypeChecker { checker }
    }

    // 统一两个类型
    pub fn unify(&mut self, t1: &Type, t2: &Type) -> TypeResult<()> {
        let t1 = self.checker.subst.apply(t1);
        let t2 = self.checker.subst.apply(t2);

        match (&t1, &t2) {
            // 相同类型直接匹配
            (a, b) if a == b => Ok(()),

            // 类型变量的情况
            (Type::Var(id), ty) | (ty, Type::Var(id)) => self.unify_var(*id, ty.clone()),

            // 数组类型
            (Type::Array(a), Type::Array(b)) => self.unify(a, b),

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
    pub fn unify_var(&mut self, var_id: TypeVarId, ty: Type) -> TypeResult<()> {
        // 检查是否已有替换
        if let Some(existing) = self.checker.subst.0.get(&var_id) {
            // 克隆现有类型以避免可变和不可变借用冲突
            let existing_type = existing.clone();
            return self.unify(&existing_type, &ty);
        }

        // 检查是否出现在类型中（防止递归类型）
        if Self::occurs_check(var_id, &ty) {
            return Err(TypeError::RecursiveType);
        }

        // 添加替换
        self.checker.subst.add(var_id, ty);
        Ok(())
    }

    // 检查类型变量是否出现在类型中
    fn occurs_check(var_id: TypeVarId, ty: &Type) -> bool {
        match ty {
            Type::Var(id) => *id == var_id,
            Type::Array(elem) => Self::occurs_check(var_id, elem),
            Type::Function(params, ret) => {
                params.iter().any(|p| Self::occurs_check(var_id, p))
                    || Self::occurs_check(var_id, ret)
            }
            Type::Record(fields) => fields.values().any(|f| Self::occurs_check(var_id, f)),
            Type::Generic(_, args) => args.iter().any(|a| Self::occurs_check(var_id, a)),
            _ => false,
        }
    }
}
