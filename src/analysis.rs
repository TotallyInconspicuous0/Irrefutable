use crate::{ast::{FunctionCase, FunctionDecl, AstType, SpanOf, Binding}, lexer::Span, types::{GlobalContext, LocalContext, Type, IntegralType}};

#[derive(Debug)]
pub struct AnalysisError {
    message: String,
    span: Span
}

impl AnalysisError {
    pub fn new<T: Into<String>>(message: T, span: &Span) -> AnalysisError {
        AnalysisError {
            message: message.into(),
            span: span.clone()
        }
    }

    pub fn message(&self) -> &str {
        &self.message
    }

    pub fn span(&self) -> &Span {
        &self.span
    }
}

pub struct Function {
    name: String,
    decl: Option<FunctionDecl>,
    cases: Vec<FunctionCase>,
}

impl Function {
    pub fn new<T: Into<String>>(name: T) -> Function {
        Function {
            name: name.into(),
            decl: None,
            cases: Vec::new()
        }
    }

    pub fn name(&self) -> &str {
        &self.name
    }

    fn bind_argument_to_locals(binding: &Binding, argument_type: &Type, context: &mut LocalContext) -> Result<(), AnalysisError> {
        match binding {
            Binding::Number(span, _) => {
                if matches!(argument_type, Type::Integral(IntegralType::Int)) {
                    Ok(())
                } else {
                    Err(AnalysisError::new("Integer binding to non-integer value", span))
                }
            }
            Binding::Name(_, name) => {
                context.add(name, argument_type.clone());
                Ok(())
            }
        }
    }

    fn check_case_against_decl(decl: &FunctionDecl, case: &FunctionCase, globals: &GlobalContext) -> Result<(), AnalysisError> {
        let mut args = Vec::new();

        let mut function_rhs = &decl.signature;
        while let AstType::Function(_, arg, rhs) = function_rhs {
            args.push(arg.to_type()?);
            function_rhs = rhs;

            if args.len() == case.bindings.len() {
                break
            }
        }
        let return_type = function_rhs.to_type()?;

        if case.bindings.len() > args.len() {
            return Err(AnalysisError::new("To many bindings for arguments", case.bindings[args.len()].span()))
        }

        let mut context = LocalContext::new(globals);

        for (binding, arg) in case.bindings.iter().zip(args.iter()) {
            Function::bind_argument_to_locals(binding, arg, &mut context)?;
        }

        let expr_type = case.expr.weakest_type(&mut context)?;

        if expr_type != return_type {
            return Err(AnalysisError::new("Expression type does not match signature", case.expr.span()))
        }

        Ok(())
    }

    pub fn typ(&self) -> Result<Type, AnalysisError> {
        if let Some(decl) = &self.decl {
            decl.signature.to_type()
        } else {
            todo!()
        }
    }

    pub fn check(&self, globals: &GlobalContext) -> Result<(), AnalysisError> {
        if let Some(decl) = &self.decl {
            for case in &self.cases {
                Function::check_case_against_decl(decl, case, globals)?;
            }

            Ok(())
        } else {
            // Infer types, check consistency
            todo!()
        }
    }

    pub fn add_decl(&mut self, decl: FunctionDecl) -> Result<(), AnalysisError> {
        if self.decl.is_some() {
            return Err(AnalysisError::new(format!("Function {} declared twice", self.name), &decl.span));
        }

        self.decl = Some(decl);
        Ok(())
    }

    pub fn add_case(&mut self, case: FunctionCase) -> Result<(), AnalysisError> {
        self.cases.push(case);

        Ok(())
    }
}