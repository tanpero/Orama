use crate::typechecker::types::{Type, TypeVarId};
use std::collections::HashMap;

// 类型替换
#[derive(Debug, Clone)]
pub struct TypeSubst(pub HashMap<TypeVarId, Type>);

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
            Type::Array(elem_ty) => Type::Array(Box::new(self.apply(elem_ty))),
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
