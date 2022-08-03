use std::collections::HashMap;

use crate::{ast::{AstType, Expr}, analysis::AnalysisError, lexer::Span};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum IntegralType {
    Int
}

// pub type TypeArgId = usize;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Type {
    Integral(IntegralType),
    Function(Box<Type>, Box<Type>),
    // TypeCall(Box<Type>, Box<Type>),
    // ForAll(TypeArgId, Option<Box<Type>>)
}

impl Type {
    fn from_name(name: &str) -> Option<Type> {
        match name {
            "Int" => Some(Type::Integral(IntegralType::Int)),
            _ => None
        }
    }
}

impl AstType {
    pub fn to_type(&self) -> Result<Type, AnalysisError> {
        match self {
            Self::Name(span, name) =>
                match Type::from_name(name) {
                    Some(t) => Ok(t),
                    None => Err(AnalysisError::new("Could not find type", span))
                },
            Self::Function(_, from, to) => Ok(Type::Function(Box::new(from.to_type()?), Box::new(to.to_type()?)))
        }
    }
}

pub struct GlobalContext {
    map: HashMap<String, Type>
}

impl GlobalContext {
    pub fn new() -> GlobalContext {
        GlobalContext {
            map: HashMap::new()
        }
    }

    pub fn find(&self, name: &str) -> Option<&Type> {
        self.map.get(name)
    }

    pub fn add(&mut self, name: &str, t: Type) {
        self.map.insert(name.to_owned(), t);
    }
}

pub struct LocalContext<'a> {
    global: &'a GlobalContext,
    map: HashMap<String, Type>
}

impl<'a> LocalContext<'a> {
    pub fn new(global: &'a GlobalContext) -> LocalContext<'a> {
        LocalContext {
            global,
            map: HashMap::new()
        }
    }

    pub fn find(&self, name: &str) -> Option<&Type> {
        if let Some(local) = self.map.get(name) {
            return Some(local)
        }

        if let Some(global) = self.global.find(name) {
            return Some(global)
        }

        None
    }

    pub fn find_err(&self, span: &Span, name: &str) -> Result<Type, AnalysisError> {
        match self.find(name) {
            Some(t) => Ok(t.clone()),
            None => Err(AnalysisError::new("Could not resolve name", span))
        }
    }

    pub fn add(&mut self, name: &str, typ: Type) {
        self.map.insert(name.to_owned(), typ);
    }
}

impl Expr {
    pub fn weakest_type(&self, context: &mut LocalContext) -> Result<Type, AnalysisError> {
        match self {
            Expr::Name(span, name) => Ok(context.find_err(span, name)?),
            Expr::Number(_, _) => Ok(Type::Integral(IntegralType::Int)), // NOTE: Make this weaker in the future
            Expr::Call(span, func, arg) => {
                let func_type = func.weakest_type(context)?;
                let arg_type = arg.weakest_type(context)?;

                match func_type {
                    Type::Function(from, to) => {
                        // TODO: Replace == with stronger than or equal to
                        if from.as_ref() == &arg_type {
                            Ok(*to)
                        } else {
                            Err(AnalysisError::new("Incorrect argument type", span))
                        }
                    }
                    _ => Err(AnalysisError::new("Not a function", span))
                }
            },
            Expr::Binary(span, lhs, rhs, _) => {
                let lhs_type = lhs.weakest_type(context)?;
                if lhs_type != rhs.weakest_type(context)? {
                    Err(AnalysisError::new("Incompatible operation", span))
                } else {
                    Ok(lhs_type)
                }
            },
            Expr::Do(_, seq) => {
                // TODO: Check chain matches up
                seq.last().unwrap().weakest_type(context)
            },
        }
    }
}