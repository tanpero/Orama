use std::collections::HashMap;
use std::fmt;

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
