use crate::lexer::{Lexer, Span, Token};

pub struct SyntaxError {
    message: String,
    span: Option<Span>
}

impl SyntaxError {
    pub fn new<T: Into<String>>(message: T, span: Option<&Span>) -> SyntaxError {
        SyntaxError {
            message: message.into(),
            span: span.cloned()
        }
    }

    pub fn message(&self) -> &str {
        &self.message
    }

    pub fn span(&self) -> Option<&Span> {
        self.span.as_ref()
    }
}

pub enum ParseResult<T> {
    NoMatch,
    Match(T),
    Err(SyntaxError)
}

#[derive(Debug)]
pub enum Type {
    Function(Box<Type>, Box<Type>),
    Name(String)
}

impl Type {
    fn parse_primary<T: Iterator<Item=char>>(lexer: &mut Lexer<T>) -> ParseResult<Type> {
        match lexer.peek(0) {
            Some(Token::Identifier(name)) => {
                let name = name.to_owned();
                lexer.step();
                ParseResult::Match(Type::Name(name))
            }
            _ => ParseResult::NoMatch
        }
    }

    fn parse_arrow<T: Iterator<Item=char>>(lexer: &mut Lexer<T>) -> ParseResult<Type> {
        let primary = match Type::parse_primary(lexer) {
            ParseResult::Err(err) => return ParseResult::Err(err),
            ParseResult::NoMatch => return ParseResult::NoMatch,
            ParseResult::Match(primary) => primary,
        };

        match lexer.peek(0) {
            Some(Token::Arrow) => {
                lexer.step();

                let rhs = match Type::parse_arrow(lexer) {
                    ParseResult::Err(err) => return ParseResult::Err(err),
                    ParseResult::NoMatch => return ParseResult::Err(SyntaxError::new("Expected rhs to arrow type", lexer.peek_span(0))),
                    ParseResult::Match(primary) => primary,
                };

                ParseResult::Match(Type::Function(Box::new(primary), Box::new(rhs)))
            }
            _ => ParseResult::Match(primary)
        }
    }

    pub fn parse<T: Iterator<Item=char>>(lexer: &mut Lexer<T>) -> ParseResult<Type> {
        Type::parse_arrow(lexer)
    }
}

#[derive(Debug)]
pub struct FunctionDecl {
    pub name: String,
    pub signature: Type
}

impl FunctionDecl {
    pub fn parse_from_name<T: Iterator<Item=char>>(name: String, lexer: &mut Lexer<T>) -> ParseResult<FunctionDecl> {
        let signature = match Type::parse(lexer) {
            ParseResult::NoMatch => return ParseResult::Err(SyntaxError::new("Could not parse type", lexer.peek_span(0))),
            ParseResult::Err(err) => return ParseResult::Err(err),
            ParseResult::Match(sig) => sig,
        };

        lexer.consume_newline();

        ParseResult::Match(FunctionDecl {
            name, signature
        })
    }
}

#[derive(Debug)]
pub enum Binding {
    Number(String),
    Name(String)
}

impl Binding {
    pub fn parse<T: Iterator<Item=char>>(lexer: &mut Lexer<T>) -> ParseResult<Binding> {
        match lexer.peek(0) {
            Some(Token::Identifier(name)) => {
                let name = name.to_owned();
                lexer.step();
                ParseResult::Match(Binding::Name(name))
            }
            Some(Token::Number(num)) => {
                let num = num.to_owned();
                lexer.step();
                ParseResult::Match(Binding::Number(num))
            }
            _ => ParseResult::NoMatch
        }
    }
}

#[derive(Debug)]
pub enum BinaryOp {
    Add, Sub
}

#[derive(Debug)]
pub enum Expr {
    Name(String),
    Number(String),
    Call(Box<Expr>, Box<Expr>),
    Binary(Box<Expr>, Box<Expr>, BinaryOp),
    Do(Vec<Expr>)
}

impl Expr {
    fn parse_primary<T: Iterator<Item=char>>(lexer: &mut Lexer<T>) -> ParseResult<Expr> {
        match lexer.peek(0) {
            Some(Token::Identifier(name)) => {
                let name = name.to_owned();
                lexer.step();
                ParseResult::Match(Expr::Name(name))
            }
            Some(Token::Number(num)) => {
                let num = num.to_owned();
                lexer.step();
                ParseResult::Match(Expr::Number(num))
            }
            Some(Token::OpenParen) => {
                lexer.step_newline();
                
                let expr = match Expr::parse(lexer) {
                    ParseResult::Err(err) => return ParseResult::Err(err),
                    ParseResult::NoMatch => return ParseResult::Err(SyntaxError::new("Expected expression", lexer.peek_span(0))),
                    ParseResult::Match(expr) => expr
                };
                
                lexer.consume_newline();
                
                match lexer.peek(0) {
                    Some(Token::CloseParen) => {},
                    _ => return ParseResult::Err(SyntaxError::new("Expected ')'", lexer.peek_span(0)))
                }

                lexer.step();

                ParseResult::Match(expr)
            }
            _ => ParseResult::NoMatch
        }
    }

    fn parse_call<T: Iterator<Item=char>>(lexer: &mut Lexer<T>) -> ParseResult<Expr> {
        let mut expr = match Expr::parse_primary(lexer) {
            ParseResult::NoMatch => return ParseResult::NoMatch,
            ParseResult::Err(err) => return ParseResult::Err(err),
            ParseResult::Match(primary) => primary
        };

        loop {
            let arg = match Expr::parse_primary(lexer) {
                ParseResult::NoMatch => return ParseResult::Match(expr),
                ParseResult::Err(err) => return ParseResult::Err(err),
                ParseResult::Match(arg) => arg
            };
    
            expr = Expr::Call(Box::new(expr), Box::new(arg));
        }
    }

    fn parse_add_sub<T: Iterator<Item=char>>(lexer: &mut Lexer<T>) -> ParseResult<Expr> {
        let lhs = match Expr::parse_call(lexer) {
            ParseResult::NoMatch => return ParseResult::NoMatch,
            ParseResult::Err(err) => return ParseResult::Err(err),
            ParseResult::Match(lhs) => lhs
        };

        let op = match lexer.peek(0) {
            Some(Token::Add) => {
                lexer.step();
                BinaryOp::Add
            }
            Some(Token::Sub) => {
                lexer.step();
                BinaryOp::Sub
            }
            _ => return ParseResult::Match(lhs)
        };

        let rhs = match Expr::parse_add_sub(lexer) {
            ParseResult::NoMatch => return ParseResult::Err(SyntaxError::new("Expected rhs", lexer.peek_span(0))),
            ParseResult::Err(err) => return ParseResult::Err(err),
            ParseResult::Match(rhs) => rhs
        };

        ParseResult::Match(Expr::Binary(Box::new(lhs), Box::new(rhs), op))
    }

    fn parse_do_then<T: Iterator<Item=char>>(lexer: &mut Lexer<T>) -> ParseResult<Expr> {
        match lexer.peek(0) {
            Some(Token::DoKeyword) => {}
            _ => return Expr::parse_add_sub(lexer)
        }
        lexer.step();

        let mut steps = Vec::new();
        loop {
            lexer.consume_newline();
            steps.push(match Expr::parse_do_then(lexer) {
                ParseResult::Err(err) => return ParseResult::Err(err),
                ParseResult::NoMatch => return ParseResult::Err(SyntaxError::new("Expected expression in do/then statement", lexer.peek_span(0))),
                ParseResult::Match(step) => step
            });
            lexer.consume_newline();

            match lexer.peek(0) {
                Some(Token::ThenKeyword) => lexer.step(),
                _ => break
            }
        }

        ParseResult::Match(Expr::Do(steps))
    }

    pub fn parse<T: Iterator<Item=char>>(lexer: &mut Lexer<T>) -> ParseResult<Expr> {
        Expr::parse_do_then(lexer)
    }
}

#[derive(Debug)]
pub struct FunctionCase {
    pub name: String,
    pub bindings: Vec<Binding>,
    pub expr: Expr
}

impl FunctionCase {
    pub fn parse_from_name<T: Iterator<Item=char>>(name: String, lexer: &mut Lexer<T>) -> ParseResult<FunctionCase> {
        let mut bindings = Vec::new();

        loop {
            match lexer.peek(0) {
                Some(Token::Eq) => {
                    lexer.step();
                    break
                },
                _ => {
                    bindings.push(match Binding::parse(lexer) {
                        ParseResult::Err(err) => return ParseResult::Err(err),
                        ParseResult::NoMatch => return ParseResult::Err(SyntaxError::new("Expected binding for argument match", lexer.peek_span(0))),
                        ParseResult::Match(binding) => binding
                    });
                }
            }
        }

        lexer.consume_newline();

        let expr = match Expr::parse(lexer) {
            ParseResult::Err(err) => return ParseResult::Err(err),
            ParseResult::NoMatch => return ParseResult::Err(SyntaxError::new("Expected expression", lexer.peek_span(0))),
            ParseResult::Match(expr) => expr
        };

        lexer.consume_newline();

        ParseResult::Match(FunctionCase {
            name, bindings, expr
        })
    }
}

#[derive(Debug)]
pub enum TopLevelDecl {
    FunctionDecl(FunctionDecl),
    FunctionDef(FunctionCase)
}

impl TopLevelDecl {
    pub fn parse<T: Iterator<Item=char>>(lexer: &mut Lexer<T>) -> ParseResult<TopLevelDecl> {
        let name = match lexer.peek(0) {
            Some(Token::Identifier(name)) => name.to_owned(),
            _ => return ParseResult::NoMatch
        };
        lexer.step();
        
        match lexer.peek(0) {
            Some(Token::DblColon) => {
                lexer.step();
                match FunctionDecl::parse_from_name(name, lexer) {
                    ParseResult::NoMatch => unreachable!(),
                    ParseResult::Match(node) => ParseResult::Match(TopLevelDecl::FunctionDecl(node)),
                    ParseResult::Err(err) => ParseResult::Err(err)
                }
            }
            _ => {
                match FunctionCase::parse_from_name(name, lexer) {
                    ParseResult::NoMatch => unreachable!(),
                    ParseResult::Match(node) => ParseResult::Match(TopLevelDecl::FunctionDef(node)),
                    ParseResult::Err(err) => ParseResult::Err(err)
                }
            }
        }
    }
}