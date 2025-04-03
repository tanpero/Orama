use std::fmt;


#[derive(Debug, Clone)]
pub enum Expr {
    // 字面量
    Literal(Literal),
    // 变量引用
    Variable(String),
    // 函数表达式
    Function(Vec<Parameter>, Box<Expr>),
    // 函数调用
    Call(Box<Expr>, Vec<Expr>),
    // 二元操作
    Binary(Box<Expr>, BinaryOp, Box<Expr>),
    // 一元操作
    Unary(UnaryOp, Box<Expr>),
    // 条件表达式
    If(Box<Expr>, Box<Expr>, Option<Box<Expr>>),
    // 块表达式
    Block(Vec<Stmt>, Option<Box<Expr>>),
    // 匹配表达式
    Match(Box<Expr>, Vec<MatchCase>),
    // 代数效应操作
    Perform(String, String, Vec<Expr>),
    // 代数效应处理
    Handle(Box<Expr>, Vec<EffectHandler>, Option<Box<ReturnHandler>>),
    // 管道表达式
    Pipe(Box<Expr>, Box<Expr>),
}

#[derive(Debug, Clone)]
pub enum Literal {
    Number(f64),
    String(String),
    Boolean(bool),
    Array(Vec<Expr>),
    Object(Vec<(String, Expr)>),
    Unit,
    Null,
}

#[derive(Debug, Clone)]
pub enum BinaryOp {
    Add,
    Subtract,
    Multiply,
    Divide,
    Modulo,
    Equal,
    NotEqual,
    Less,
    Greater,
    LessEqual,
    GreaterEqual,
    And,
    Or,
    Access, // 新增：用于属性访问
}

#[derive(Debug, Clone)]
pub enum UnaryOp {
    Negate,
    Not,
}

#[derive(Debug, Clone)]
pub struct Parameter {
    pub name: String,
    pub type_annotation: Option<TypeAnnotation>,
}

#[derive(Debug, Clone)]
pub struct MatchCase {
    pub pattern: Pattern,
    pub body: Expr,
}

#[derive(Debug, Clone)]
pub struct Pattern {
    pub name: String,
    pub params: Option<Vec<Pattern>>,
}

#[derive(Debug, Clone)]
pub struct EffectHandler {
    pub effect_name: String,
    pub operations: Vec<EffectOperation>,
}

#[derive(Debug, Clone)]
pub struct EffectOperation {
    pub name: String,
    pub params: Vec<Parameter>,
    pub body: Expr,
}

#[derive(Debug, Clone)]
pub struct ReturnHandler {
    pub param: String,
    pub body: Box<Expr>,
}

#[derive(Debug, Clone)]
pub enum Stmt {
    // 变量声明
    VariableDecl(String, Expr),
    // 函数声明
    FunctionDecl(String, Vec<Parameter>, Expr),
    // 效应声明
    EffectDecl(String, Option<Vec<String>>, Vec<EffectSignature>),
    // 类型声明
    TypeDecl(String, Option<Vec<String>>, TypeDefinition),
    // 表达式语句
    Expression(Expr),
}

#[derive(Debug, Clone)]
pub struct EffectSignature {
    pub name: String,
    pub function_type: FunctionType,
}

#[derive(Debug, Clone)]
pub enum TypeDefinition {
    Union(Vec<TypeVariant>),
    Record(Vec<TypeField>),
}

#[derive(Debug, Clone)]
pub struct TypeVariant {
    pub name: String,
    pub params: Option<Vec<Parameter>>,
}

#[derive(Debug, Clone)]
pub struct TypeField {
    pub name: String,
    pub type_annotation: TypeAnnotation,
}

#[derive(Debug, Clone)]
pub enum TypeAnnotation {
    Simple(String, Option<Vec<TypeAnnotation>>),
    Function(FunctionType),
    Effect(Vec<String>, Box<TypeAnnotation>),
    Array(Box<TypeAnnotation>),
}

#[derive(Debug, Clone)]
pub struct FunctionType {
    pub params: Vec<TypeAnnotation>,
    pub return_type: Box<TypeAnnotation>,
}

#[derive(Debug, Clone)]
pub struct Program {
    pub statements: Vec<Stmt>,
}

impl fmt::Display for Program {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        for stmt in &self.statements {
            writeln!(f, "{:?}", stmt)?;
        }
        Ok(())
    }
}