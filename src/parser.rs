use crate::{lexer::{Lexer, Span, Token}, ast::{AstType, Binding, Expr, BinaryOp, FunctionDecl, FunctionCase, TopLevelDecl}};

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

impl AstType {
    fn parse_primary<T: Iterator<Item=char>>(lexer: &mut Lexer<T>) -> ParseResult<AstType> {
        match lexer.peek(0) {
            Some(Token::Identifier(name)) => {
                let name = name.to_owned();
                let span = lexer.peek_span(0).unwrap().clone();
                lexer.step();
                ParseResult::Match(AstType::Name(span, name))
            }
            _ => ParseResult::NoMatch
        }
    }

    fn parse_arrow<T: Iterator<Item=char>>(lexer: &mut Lexer<T>) -> ParseResult<AstType> {
        let call_start = lexer.peek_span(0).cloned();

        let primary = match AstType::parse_primary(lexer) {
            ParseResult::Err(err) => return ParseResult::Err(err),
            ParseResult::NoMatch => return ParseResult::NoMatch,
            ParseResult::Match(primary) => primary,
        };

        match lexer.peek(0) {
            Some(Token::Arrow) => {
                lexer.step();

                let rhs = match AstType::parse_arrow(lexer) {
                    ParseResult::Err(err) => return ParseResult::Err(err),
                    ParseResult::NoMatch => return ParseResult::Err(SyntaxError::new("Expected rhs to arrow type", lexer.peek_span(0))),
                    ParseResult::Match(primary) => primary,
                };

                let mut call_span = call_start.unwrap();
                call_span.extend_to(lexer.tell());

                ParseResult::Match(AstType::Function(call_span, Box::new(primary), Box::new(rhs)))
            }
            _ => ParseResult::Match(primary)
        }
    }

    pub fn parse<T: Iterator<Item=char>>(lexer: &mut Lexer<T>) -> ParseResult<AstType> {
        AstType::parse_arrow(lexer)
    }
}

impl FunctionDecl {
    pub fn parse_from_name<T: Iterator<Item=char>>(name: String, mut start: Span, lexer: &mut Lexer<T>) -> ParseResult<FunctionDecl> {
        let signature = match AstType::parse(lexer) {
            ParseResult::NoMatch => return ParseResult::Err(SyntaxError::new("Could not parse type", lexer.peek_span(0))),
            ParseResult::Err(err) => return ParseResult::Err(err),
            ParseResult::Match(sig) => sig,
        };
        start.extend_to(lexer.tell());

        lexer.consume_newline();

        ParseResult::Match(FunctionDecl {
            span: start, name, signature
        })
    }
}

impl Binding {
    pub fn parse<T: Iterator<Item=char>>(lexer: &mut Lexer<T>) -> ParseResult<Binding> {
        match lexer.peek(0) {
            Some(Token::Identifier(name)) => {
                let name = name.to_owned();
                let span = lexer.peek_span(0).unwrap().clone();
                lexer.step();
                ParseResult::Match(Binding::Name(span, name))
            }
            Some(Token::Number(num)) => {
                let num = num.to_owned();
                let span = lexer.peek_span(0).unwrap().clone();
                lexer.step();
                ParseResult::Match(Binding::Number(span, num))
            }
            _ => ParseResult::NoMatch
        }
    }
}

impl Expr {
    fn parse_primary<T: Iterator<Item=char>>(lexer: &mut Lexer<T>) -> ParseResult<Expr> {
        match lexer.peek(0) {
            Some(Token::Identifier(name)) => {
                let name = name.to_owned();
                let span = lexer.peek_span(0).unwrap().clone();
                lexer.step();
                ParseResult::Match(Expr::Name(span, name))
            }
            Some(Token::Number(num)) => {
                let num = num.to_owned();
                let span = lexer.peek_span(0).unwrap().clone();
                lexer.step();
                ParseResult::Match(Expr::Number(span, num))
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
        let span = lexer.peek_span(0).cloned();

        let mut expr = match Expr::parse_primary(lexer) {
            ParseResult::NoMatch => return ParseResult::NoMatch,
            ParseResult::Err(err) => return ParseResult::Err(err),
            ParseResult::Match(primary) => primary
        };

        let mut span = span.unwrap();
        loop {
            let arg = match Expr::parse_primary(lexer) {
                ParseResult::NoMatch => return ParseResult::Match(expr),
                ParseResult::Err(err) => return ParseResult::Err(err),
                ParseResult::Match(arg) => arg
            };

            span.extend_to(lexer.tell());
    
            expr = Expr::Call(span.clone(), Box::new(expr), Box::new(arg));
        }
    }

    fn parse_add_sub<T: Iterator<Item=char>>(lexer: &mut Lexer<T>) -> ParseResult<Expr> {
        let span = lexer.peek_span(0).cloned();

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

        let mut span = span.unwrap();
        span.extend_to(lexer.tell());

        ParseResult::Match(Expr::Binary(span, Box::new(lhs), Box::new(rhs), op))
    }

    fn parse_do_then<T: Iterator<Item=char>>(lexer: &mut Lexer<T>) -> ParseResult<Expr> {
        match lexer.peek(0) {
            Some(Token::DoKeyword) => {}
            _ => return Expr::parse_add_sub(lexer)
        }
        let mut span = lexer.peek_span(0).unwrap().clone();
        lexer.step();

        let mut steps = Vec::new();
        loop {
            lexer.consume_newline();
            steps.push(match Expr::parse_do_then(lexer) {
                ParseResult::Err(err) => return ParseResult::Err(err),
                ParseResult::NoMatch => return ParseResult::Err(SyntaxError::new("Expected expression in do/then statement", lexer.peek_span(0))),
                ParseResult::Match(step) => step
            });
            span.extend_to(lexer.tell());
            lexer.consume_newline();

            match lexer.peek(0) {
                Some(Token::ThenKeyword) => lexer.step(),
                _ => break
            }
        }

        ParseResult::Match(Expr::Do(span, steps))
    }

    pub fn parse<T: Iterator<Item=char>>(lexer: &mut Lexer<T>) -> ParseResult<Expr> {
        Expr::parse_do_then(lexer)
    }
}

impl FunctionCase {
    pub fn parse_from_name<T: Iterator<Item=char>>(name: String, mut span: Span, lexer: &mut Lexer<T>) -> ParseResult<FunctionCase> {
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
        span.extend_to(lexer.tell());

        lexer.consume_newline();

        ParseResult::Match(FunctionCase {
            span, name, bindings, expr
        })
    }
}

impl TopLevelDecl {
    pub fn parse<T: Iterator<Item=char>>(lexer: &mut Lexer<T>) -> ParseResult<TopLevelDecl> {
        let name = match lexer.peek(0) {
            Some(Token::Identifier(name)) => name.to_owned(),
            _ => return ParseResult::NoMatch
        };
        let span = lexer.peek_span(0).unwrap().clone();
        lexer.step();
        
        match lexer.peek(0) {
            Some(Token::DblColon) => {
                lexer.step();
                match FunctionDecl::parse_from_name(name, span, lexer) {
                    ParseResult::NoMatch => unreachable!(),
                    ParseResult::Match(node) => ParseResult::Match(TopLevelDecl::FunctionDecl(node)),
                    ParseResult::Err(err) => ParseResult::Err(err)
                }
            }
            _ => {
                match FunctionCase::parse_from_name(name, span, lexer) {
                    ParseResult::NoMatch => unreachable!(),
                    ParseResult::Match(node) => ParseResult::Match(TopLevelDecl::FunctionDef(node)),
                    ParseResult::Err(err) => ParseResult::Err(err)
                }
            }
        }
    }
}