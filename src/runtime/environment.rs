// 获取所有绑定
    pub fn get_bindings(&self) -> impl Iterator<Item = (&String, &Value)> {
        self.values.iter()
    }