use std;

const BUFLEN: usize = 256;

#[derive(PartialEq)]
pub enum Token {
    Ident(String),
    Punct(char),
    Int(i32),
    Char(char),
    Str(String),
}

use self::Token::*;

impl std::fmt::Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> Result<(), std::fmt::Error> {
        match self {
            &Ident(ref s) => write!(f, "{}", s),
            &Punct(c) | &Char(c) => write!(f, "'{}'", c),
            &Int(i) => write!(f, "{}", i),
            &Str(ref s) => write!(f, "\"{}\"", s),
        }
    }
}

pub struct Lex {
    buf: Vec<u8>,
    p: usize,
    ungotten: Option<Token>,
}

impl Lex {
    pub fn new(buf: Vec<u8>) -> Lex {
        Lex {
            buf: buf,
            p: 0,
            ungotten: None,
        }
    }

    fn getc(&mut self) -> Option<char> {
        if self.p >= self.buf.len() {
            return None;
        }
        let res = self.buf[self.p];
        self.p += 1;
        Some(res as char)
    }

    fn ungetc(&mut self) {
        self.p -= 1;
    }

    fn skip_space(&mut self) {
        while let Some(c) = self.getc() {
            if !c.is_whitespace() {
                self.ungetc();
                return;
            }
        }
    }

    fn read_number(&mut self, mut n: u32) -> Token {
        while let Some(c) = self.getc() {
            if let Some(k) = c.to_digit(10) {
                n = n * 10 + k;
            } else {
                self.ungetc();
                break;
            }
        }
        return Token::Int(n as i32);
    }

    fn read_char(&mut self) -> Token {
        let err = || panic!("Unterminated char");
        let mut c = self.getc().unwrap_or_else(&err);
        if c == '\\' {
            c = self.getc().unwrap_or_else(&err);
        }
        let c2 = self.getc().unwrap_or_else(&err);
        if c2 != '\'' {
            panic!("Malformed char constant");
        }
        Token::Char(c)
    }

    fn read_string(&mut self) -> Token {
        let mut buf = String::new();
        loop {
            let mut c = self.getc().expect("Unterminated string");
            if c == '"' {
                break;
            }
            if c == '\\' {
                c = self.getc().expect("Unterminated \\");
            }
            buf.push(c);
            if buf.len() >= BUFLEN {
                panic!("String too long");
            }
        }
        Token::Str(buf)
    }

    fn read_ident(&mut self, c: char) -> Token {
        let mut name = String::new();
        name.push(c);
        loop {
            let c = self.getc().expect("Unexpected EOF");
            if !c.is_alphanumeric() {
                self.ungetc();
                return Token::Ident(name);
            }
            name.push(c);
            if name.len() >= BUFLEN {
                panic!("Identifier too long");
            }
        }
    }

    fn read_token_int(&mut self) -> Option<Token> {
        self.skip_space();
        self.getc().map(|c| match c {
            '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' => {
                self.read_number(c.to_digit(10).unwrap())
            }
            '"' => self.read_string(),
            '\'' => self.read_char(),
            'a' | 'b' | 'c' | 'd' | 'e' | 'f' | 'g' | 'h' | 'i' | 'j' | 'k' | 'l' | 'm' | 'n' |
            'o' | 'p' | 'q' | 'r' | 's' | 't' | 'u' | 'v' | 'w' | 'x' | 'y' | 'z' | 'A' | 'B' |
            'C' | 'D' | 'E' | 'F' | 'G' | 'H' | 'I' | 'J' | 'K' | 'L' | 'M' | 'N' | 'O' | 'P' |
            'Q' | 'R' | 'S' | 'T' | 'U' | 'V' | 'W' | 'X' | 'Y' | 'Z' | '_' => self.read_ident(c),
            '/' | '=' | '*' | '+' | '-' | '(' | ')' | ',' | ';' => Punct(c),
            _ => panic!("Unexpected charaacter: '{}'", c),
        })
    }

    pub fn unget_token(&mut self, tok: Token) {
        if self.ungotten.is_some() {
            panic!("Push back buffer is already full.");
        }
        self.ungotten = Some(tok);
    }

    pub fn read_token(&mut self) -> Option<Token> {
        if let Some(_) = self.ungotten {
            let mut res = None;
            std::mem::swap(&mut self.ungotten, &mut res);
            return res;
        }
        self.read_token_int()
    }
}
