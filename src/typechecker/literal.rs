use crate::ast::Literal;
use crate::typechecker::types::Type;
use crate::typechecker::error::{TypeError, TypeResult};
use crate::typechecker::checker::TypeChecker;
use std::collections::HashMap;

// Regular struct without generic mutability
pub struct LiteralTypeChecker<'a> {
    checker: &'a TypeChecker,
}

// Implementation for immutable reference
impl<'a> LiteralTypeChecker<'a> {
    pub fn new(checker: &'a TypeChecker) -> Self {
        LiteralTypeChecker { checker }
    }

    // Check literal type (immutable version)
    pub fn check_literal(&self, lit: &Literal) -> TypeResult<Type> {
        match lit {
            Literal::Unit => Ok(Type::Null),
            Literal::Number(_) => Ok(Type::Number),
            Literal::String(_) => Ok(Type::String),
            Literal::Boolean(_) => Ok(Type::Boolean),
            Literal::Null => Ok(Type::Null),
            Literal::Array(elements) => {
                if elements.is_empty() {
                    // Empty array, return Any type array
                    return Ok(Type::Array(Box::new(Type::Any)));
                }
                
                // Check first element type
                let first_type = self.checker.check_expr(&elements[0])?;
                
                // Check all other elements match the first type
                for element in elements.iter().skip(1) {
                    let element_type = self.checker.check_expr(element)?;
                    if element_type != first_type {
                        return Err(TypeError::TypeMismatch {
                            expected: first_type.to_string(),
                            actual: element_type.to_string(),
                        });
                    }
                }
                
                Ok(Type::Array(Box::new(first_type)))
            },
            Literal::Object(fields) => {
                let mut field_types = HashMap::new();
                for (name, value) in fields {
                    let value_type = self.checker.check_expr(value)?;
                    field_types.insert(name.clone(), value_type);
                }
                Ok(Type::Record(field_types))
            },
        }
    }
}

// Separate struct for mutable reference
pub struct MutableLiteralTypeChecker<'a> {
    checker: &'a mut TypeChecker,
}

// Implementation for mutable reference
impl<'a> MutableLiteralTypeChecker<'a> {
    pub fn new(checker: &'a mut TypeChecker) -> Self {
        MutableLiteralTypeChecker { checker }
    }

    // Infer literal type (mutable version)
    pub fn infer_literal(&mut self, lit: &Literal) -> TypeResult<Type> {
        match lit {
            Literal::Unit => Ok(Type::Null),
            Literal::Number(_) => Ok(Type::Number),
            Literal::String(_) => Ok(Type::String),
            Literal::Boolean(_) => Ok(Type::Boolean),
            Literal::Null => Ok(Type::Null),
            Literal::Array(elements) => {
                if elements.is_empty() {
                    // Empty array, create type variable
                    let elem_type = self.checker.env.new_type_var();
                    Ok(Type::Array(Box::new(elem_type)))
                } else {
                    // Infer first element type
                    let first_type = self.checker.infer_expr(&elements[0])?;
                    
                    // Ensure all elements have consistent types
                    for elem in elements.iter().skip(1) {
                        let elem_type = self.checker.infer_expr(elem)?;
                        self.checker.unify(&first_type, &elem_type)?;
                    }
                    
                    Ok(Type::Array(Box::new(first_type)))
                }
            }
            Literal::Object(fields) => {
                let mut field_types = HashMap::new();
                
                for (name, value) in fields {
                    let value_type = self.checker.infer_expr(value)?;
                    field_types.insert(name.clone(), value_type);
                }
                
                Ok(Type::Record(field_types))
            }
        }
    }
}