#![feature(box_syntax, box_patterns)]

use std::io::Read;

static BUFLEN: usize = 256;

enum Ast {
    OpInt(String, Box<Ast>, Box<Ast>),
    Int(i32),
    Str(String),
}

use Ast::{OpInt, Int, Str};

impl std::fmt::Display for Ast {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> Result<(), std::fmt::Error> {
        match self {
            &OpInt(ref op, ref l, ref r) => {
                let c = if op == "add" { '+' } else { '-' };
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

fn emit_intexpr(a: Ast) {
    match a {
        OpInt(op, box l, box r) => {
            emit_intexpr(l);
            println!("\tmov %eax, %ebx");
            emit_intexpr(r);
            println!("\t{} %ebx, %eax", op);
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

    fn unget(&mut self) {
        self.p -= 1;
    }

    fn skip_space(&mut self) {
        while let Some(c) = self.getc() {
            if !c.is_whitespace() {
                self.unget();
                return;
            }
        }
    }

    fn read_number(&mut self, mut n: i32) -> Ast {
        while let Some(c) = self.getc() {
            if let Some(k) = c.to_digit(10) {
                n = n * 10 + k as i32;
            } else {
                self.unget();
                break;
            }
        }
        return Int(n);
    }

    fn read_expr2(&mut self, left: Ast) -> Ast {
        self.skip_space();
        match self.getc() {
            None => {
                return left;
            }
            Some(c) => {
                let op;
                if c == '+' {
                    op = "add";
                } else if c == '-' {
                    op = "sub";
                } else {
                    panic!("Operator expected, but got '{}'", c);
                }
                self.skip_space();
                let left = OpInt(String::from(op), Box::new(left), Box::new(self.read_prim()));
                return self.read_expr2(left);
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
        let left = self.read_prim();
        return self.read_expr2(left);
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
