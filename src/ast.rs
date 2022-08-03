use std::fmt::Write;

use crate::lexer::Span;

pub trait SpanOf {
    fn span(&self) -> &Span;
}

pub trait AstToCodeString {
    fn to_code_string(&self) -> String;
}

#[derive(Debug)]
pub enum AstType {
    Function(Span, Box<AstType>, Box<AstType>),
    Name(Span, String)
}

impl AstToCodeString for AstType {
    fn to_code_string(&self) -> String {
        match self {
            Self::Function(_, obj, arg) => format!("{} -> {}", obj.to_code_string(), arg.to_code_string()),
            Self::Name(_, name) => name.to_owned()
        }
    }
}

impl SpanOf for AstType {
    fn span(&self) -> &Span {
        match self {
            Self::Function(span, _, _) | Self::Name(span, _) => span
        }
    }
}

#[derive(Debug)]
pub enum Binding {
    Number(Span, String),
    Name(Span, String)
}

impl AstToCodeString for Binding {
    fn to_code_string(&self) -> String {
        match self {
            Self::Name(_, string) | Self::Number(_, string) => string.to_owned()
        }
    }
}

impl SpanOf for Binding {
    fn span(&self) -> &Span {
        match self {
            Self::Number(span, _) | Self::Name(span, _) => span
        }
    }
}

#[derive(Debug)]
pub enum BinaryOp {
    Add, Sub
}

impl AstToCodeString for BinaryOp {
    fn to_code_string(&self) -> String {
        match self {
            Self::Add => "+".to_owned(),
            Self::Sub => "-".to_owned(),
        }
    }
}

#[derive(Debug)]
pub enum Expr {
    Name(Span, String),
    Number(Span, String),
    Call(Span, Box<Expr>, Box<Expr>),
    Binary(Span, Box<Expr>, Box<Expr>, BinaryOp),
    Do(Span, Vec<Expr>)
}

impl AstToCodeString for Expr {
    fn to_code_string(&self) -> String {
        match self {
            Self::Name(_, string) | Self::Number(_, string) => string.to_owned(),
            Self::Call(_, func, arg) => format!("{} {}", func.to_code_string(), arg.to_code_string()),
            Self::Binary(_, lhs, rhs, op) => format!("({} {} {})", lhs.to_code_string(), op.to_code_string(), rhs.to_code_string()),
            Self::Do(_, exprs) => {
                let mut code = format!("do {}", exprs[0].to_code_string());
                for expr in &exprs[1..] {
                    write!(code, " then {}", expr.to_code_string()).unwrap();
                }
                code
            }
        }
    }
}

impl SpanOf for Expr {
    fn span(&self) -> &Span {
        match self {
            Self::Name(span, _) | Self::Number(span, _) |
            Self::Call(span, _, _) | Self::Binary(span, _, _, _) | Self::Do(span, _) => span
        }
    }
}

#[derive(Debug)]
pub struct FunctionCase {
    pub span: Span,
    pub name: String,
    pub bindings: Vec<Binding>,
    pub expr: Expr
}

impl AstToCodeString for FunctionCase {
    fn to_code_string(&self) -> String {
        let mut string = self.name.to_owned();
        
        for binding in &self.bindings {
            string.push(' ');
            string.push_str(&binding.to_code_string());
        }
        
        string.push_str(" = ");
        string.push_str(&self.expr.to_code_string());

        string
    }
}

impl SpanOf for FunctionCase {
    fn span(&self) -> &Span {
        &self.span
    }
}

#[derive(Debug)]
pub enum TopLevelDecl {
    FunctionDecl(FunctionDecl),
    FunctionDef(FunctionCase)
}

impl AstToCodeString for TopLevelDecl {
    fn to_code_string(&self) -> String {
        match self {
            Self::FunctionDecl(decl) => decl.to_code_string(),
            Self::FunctionDef(def) => def.to_code_string()
        }
    }
}

impl SpanOf for TopLevelDecl {
    fn span(&self) -> &Span {
        match self {
            Self::FunctionDecl(decl) => decl.span(),
            Self::FunctionDef(def) => def.span(),
        }
    }
}

#[derive(Debug)]
pub struct FunctionDecl {
    pub span: Span,
    pub name: String,
    pub signature: AstType
}

impl AstToCodeString for FunctionDecl {
    fn to_code_string(&self) -> String {
        let mut string = self.name.to_owned();
        string.push_str(" :: ");
        string.push_str(&self.signature.to_code_string());

        string
    }
}

impl SpanOf for FunctionDecl {
    fn span(&self) -> &Span {
        &self.span
    }
}