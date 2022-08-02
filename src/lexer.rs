use std::{iter::Peekable, collections::VecDeque};

#[derive(Debug)]
pub enum Token {
    Identifier(String),
    Number(String),
    Eq,
    DblColon,
    Arrow,
    OpenParen, CloseParen,
    Add, Sub,
    NewLine,
    DoKeyword, ThenKeyword,
    Char(char),
}

#[derive(Debug, Clone)]
pub struct Span {
    pub start: usize,
    pub length: usize
}

pub struct IoBytesCharReader<T: Iterator<Item=Result<u8, std::io::Error>>> {
    bytes: Peekable<T>
}

impl<T: Iterator<Item=Result<u8, std::io::Error>>> IoBytesCharReader<T> {
    pub fn new(bytes: T) -> IoBytesCharReader<T> {
        IoBytesCharReader {
            bytes: bytes.peekable()
        }
    }

    fn next_codepoint(&mut self) -> Option<u32> {
        let mut bytes = [0u8; 4];
        bytes[0] = self.bytes.next()?.unwrap();

        for i in 1..bytes[0].leading_ones() as usize {
            let byte = match self.bytes.next() {
                Some(byte) => *byte.as_ref().unwrap(),
                None => panic!("Invalid byte sequence (early EOF)")
            };

            if byte & 0b11000000 != 0b10000000 {
                panic!("Invalid byte sequence (invalid start)")
            }

            bytes[i] = byte;
        }

        Some(u32::from_ne_bytes(bytes))
    }
}

impl<T: Iterator<Item=Result<u8, std::io::Error>>> Iterator for IoBytesCharReader<T> {
    type Item = char;

    fn next(&mut self) -> Option<Self::Item> {
        unsafe {
            Some(char::from_u32_unchecked(self.next_codepoint()?))
        }
    }
}

pub struct Lexer<T: Iterator<Item=char>> {
    chars: Peekable<T>,
    chars_offset: usize,
    buffered: VecDeque<(Token, Span)>,
}

impl<T: Iterator<Item=char>> Lexer<T> {
    pub fn new(chars: T) -> Lexer<T> {
        Lexer {
            chars: chars.peekable(),
            chars_offset: 0,
            buffered: VecDeque::new(),
        }
    }

    fn lex_ident(&mut self) -> Option<(Token, Span)> {
        let start = self.chars_offset;
        let mut word = String::new();

        while let Some('a'..='z' | 'A'..='Z' | '_' | '0'..='9') = self.chars.peek() {
            word.push(self.chars.next().unwrap());
            self.chars_offset += 1;
        }

        Some((match word.as_str() {
            "do" => Token::DoKeyword,
            "then" => Token::ThenKeyword,
            _ => Token::Identifier(word)
        }, Span { start, length: self.chars_offset - start }))
    }

    fn lex_number(&mut self) -> Option<(Token, Span)> {
        let start = self.chars_offset;
        let mut word = String::new();

        while let Some('0'..='9' | '.' | '_') = self.chars.peek() {
            word.push(self.chars.next().unwrap());
            self.chars_offset += 1;
        }

        Some((Token::Number(word), Span { start, length: self.chars_offset - start }))
    }

    fn lex(&mut self) -> Option<(Token, Span)> {
        while let Some(chr) = self.chars.peek() {
            if chr.is_whitespace() && *chr != '\n' {
                self.chars.next();
                self.chars_offset += 1;
                continue;
            }

            if *chr == '#' {
                while let Some(chr) = self.chars.peek() {
                    if *chr == '\n' {
                        break
                    }

                    self.chars.next();
                }
                continue;
            }

            break;
        }

        match self.chars.peek() {
            None => None,
            Some('a'..='z' | 'A'..='Z' | '_') => self.lex_ident(),
            Some('0'..='9') => self.lex_number(),
            Some('\n') => {
                self.chars_offset += 1;
                self.chars.next();
                Some((Token::NewLine, Span { start: self.chars_offset - 1, length: 1 }))
            }
            Some('=') => {
                self.chars_offset += 1;
                self.chars.next();
                Some((Token::Eq, Span { start: self.chars_offset - 1, length: 1 }))
            }
            Some('(') => {
                self.chars_offset += 1;
                self.chars.next();
                Some((Token::OpenParen, Span { start: self.chars_offset - 1, length: 1 }))
            }
            Some(')') => {
                self.chars_offset += 1;
                self.chars.next();
                Some((Token::CloseParen, Span { start: self.chars_offset - 1, length: 1 }))
            }
            Some('+') => {
                self.chars_offset += 1;
                self.chars.next();
                Some((Token::Add, Span { start: self.chars_offset - 1, length: 1 }))
            }
            Some(':') => {
                self.chars.next();
                match self.chars.peek() {
                    Some(':') => {
                        self.chars.next();
                        self.chars_offset += 2;
                        Some((Token::DblColon, Span { start: self.chars_offset - 2, length: 2 }))
                    }
                    _ => {
                        self.chars_offset += 1;
                        Some((Token::Char(':'), Span { start: self.chars_offset - 1, length: 1 }))
                    }
                }
            }
            Some('-') => {
                self.chars.next();
                match self.chars.peek() {
                    Some('>') => {
                        self.chars.next();
                        self.chars_offset += 2;
                        Some((Token::Arrow, Span { start: self.chars_offset - 2, length: 2 }))
                    }
                    _ => {
                        self.chars_offset += 1;
                        Some((Token::Sub, Span { start: self.chars_offset - 1, length: 1 }))
                    }
                }
            }
            _ => {
                self.chars_offset += 1;
                Some((Token::Char(self.chars.next().unwrap()), Span { start: self.chars_offset - 1, length: 1 }))
            }
        }
    }

    fn buffer_up_to(&mut self, offset: usize) {
        if offset < self.buffered.len() {
            return;
        }

        for _ in self.buffered.len()..=offset {
            if let Some(pair) = self.lex() {
                self.buffered.push_back(pair);
            } else {
                return;
            }
        }
    }

    pub fn peek(&mut self, offset: usize) -> Option<&Token> {
        self.buffer_up_to(offset);

        if offset < self.buffered.len() {
            Some(&self.buffered[offset].0)
        } else {
            None
        }
    }

    pub fn peek_span(&mut self, offset: usize) -> Option<&Span> {
        self.buffer_up_to(offset);

        if offset < self.buffered.len() {
            Some(&self.buffered[offset].1)
        } else {
            None
        }
    }

    pub fn step(&mut self) {
        self.buffer_up_to(0);

        if self.buffered.len() > 0 {
            self.buffered.pop_front();
        }
    }

    pub fn step_newline(&mut self) {
        self.step();

        while let Some(Token::NewLine) = self.peek(0) {
            self.step();
        }
    }

    pub fn consume_newline(&mut self) {
        while let Some(Token::NewLine) = self.peek(0) {
            self.step();
        }
    }
}

#[test]
fn lex() {
    use std::str::Chars;

    let string = "Hello World!";
    let mut lexer = Lexer::<Chars>::new(string.chars());

    println!("{:?}", lexer.peek(0));
    println!("{:?}", lexer.peek(1));
    lexer.step();
    println!("{:?}", lexer.peek(0));
    println!("{:?}", lexer.peek(1));
    println!("{:?}", lexer.peek(2));
    println!("{:?}", lexer.peek(3));
}