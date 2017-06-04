#![feature(box_syntax, box_patterns)]

use std::io::Read;

static BUFLEN: usize = 256;

enum Ast {
    OpInt(char, Box<Ast>, Box<Ast>),
    Int(i32),
    Str(String),
}

use Ast::{OpInt, Int, Str};

impl std::fmt::Display for Ast {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> Result<(), std::fmt::Error> {
        match self {
            &OpInt(c, ref l, ref r) => {
                write!(f, "({} ", c)?;
                l.fmt(f)?;
                write!(f, " ")?;
                r.fmt(f)?;
                write!(f, ")")?;
            }
            &Int(i) => {
                write!(f, "{}", i)?;
            }
            &Str(ref s) => {
                write!(f, "{}", s)?;
            }
        }
        Ok(())
    }
}


fn print_quote(s: String) {
    for c in s.chars() {
        if c == '\"' || c == '\\' {
            print!("{}", '\\');
        }
        print!("{}", c);
    }
}

fn emit_string(a: Ast) {
    if let Str(s) = a {
        println!("\t.data");
        println!(".mydata:");
        print!(".string \"");
        print_quote(s);
        println!("\"");
        println!("\t.text");
        println!("\t.global stringfn");
        println!("stringfn:");
        println!("lea .mydata(%rip), %rax");
        println!("ret");
        return;
    }
    panic!("want: Str, got: {}", a);
}

fn priority(op: char) -> i32 {
    match op {
        '+' | '-' => 1,
        '*' | '/' => 2,
        _ => panic!("Unknown binary operator: {}", op),
    }
}

fn op(c: char) -> String {
    match c {
        '+' => "add".into(),
        '-' => "sub".into(),
        '*' => "imul".into(),
        _ => panic!("Should not reach: {}", c),
    }
}

fn emit_intexpr(a: Ast) {
    match a {
        OpInt(c, box l, box r) => {
            emit_intexpr(l);
            println!("\tpush %rax");
            emit_intexpr(r);
            if c == '/' {
                println!("\tmov %eax, %ebx");
                println!("\tpop %rax");
                println!("\tmov $0, %edx");
                println!("\tidiv %ebx");
            } else {
                println!("\tpop %rbx");
                if c == '-' {
                    println!("\t{} %eax, %ebx", op(c));
                } else {
                    println!("\t{} %ebx, %eax", op(c));
                }
            }
        }
        Int(n) => {
            println!("\tmov ${}, %eax", n);
        }
        _ => {
            panic!("Unexpected token");
        }
    }
}

fn compile(a: Ast) {
    if let Str(_) = a {
        emit_string(a);
        return;
    }
    println!(".text");
    println!("\t.global intfn");
    println!("intfn:");
    emit_intexpr(a);
    println!("\tret");
}

struct R {
    buf: Vec<u8>,
    p: usize,
}

impl R {
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

    fn read_number(&mut self, mut n: i32) -> Ast {
        while let Some(c) = self.getc() {
            if let Some(k) = c.to_digit(10) {
                n = n * 10 + k as i32;
            } else {
                self.ungetc();
                break;
            }
        }
        return Int(n);
    }

    fn read_expr2(&mut self, prec: i32) -> Ast {
        let mut ast = self.read_prim();
        loop {
            self.skip_space();
            match self.getc() {
                None => {
                    return ast;
                }
                Some(c) => {
                    let prec2 = priority(c);
                    if prec2 < prec {
                        self.ungetc();
                        return ast;
                    }
                    self.skip_space();
                    ast = OpInt(c, Box::new(ast), Box::new(self.read_expr2(prec2 + 1)));
                }
            }
        }
    }


    fn read_string(&mut self) -> Ast {
        let mut s = String::new();
        while let Some(c) = self.getc() {
            let mut c = c;
            if c == '"' {
                return Str(s);
            } else if c == '\\' {
                c = self.getc().expect("Unterminated \\");
            }
            s.push(c);
            if s.len() + 1 >= BUFLEN {
                panic!("String too long");
            }
        }
        panic!("Unterminated string");
    }

    fn read_prim(&mut self) -> Ast {
        let c = self.getc().expect("Unexpected EOF");
        if let Some(k) = c.to_digit(10) {
            return self.read_number(k as i32);
        } else if c == '"' {
            return self.read_string();
        } else {
            panic!("Don't know how to handle '{}'", c);
        }
    }

    fn read_expr(&mut self) -> Ast {
        return self.read_expr2(0);
    }
}

fn main() {
    let mut buf = vec![];
    std::io::stdin().read_to_end(&mut buf).unwrap();
    let mut r = R { buf: buf, p: 0 };
    let a = r.read_expr();

    if std::env::args().nth(1) == Some(String::from("-a")) {
        println!("{}", a);
    } else {
        compile(a);
    }
}
