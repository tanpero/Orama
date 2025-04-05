use crate::typechecker::types::{Type, TypeVarId};
use std::collections::{HashMap, HashSet};

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

    // 添加递归类型定义
    pub fn add_recursive_type(&mut self, name: String, type_params: Vec<String>, type_def: Type) {
        // 先添加一个占位符类型
        let placeholder = Type::Generic(
            name.clone(),
            type_params
                .iter()
                .map(|p| Type::Var(TypeVarId(0)))
                .collect(),
        );
        self.types.insert(name.clone(), placeholder);

        // 然后添加实际类型定义
        self.types.insert(name, type_def);
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
