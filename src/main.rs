#![feature(box_syntax, box_patterns)]

use std::io::Read;

static BUFLEN: usize = 256;

enum Ast {
    OpInt(char, Box<Ast>, Box<Ast>),
    Int(i32),
    Sym(String, usize), // name, pos.
}

#[derive(Clone)]
struct Var {
    name: String,
    pos: usize,
}

use Ast::{OpInt, Int, Sym};

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
            &Sym(ref name, _) => {
                write!(f, "{}", name)?;
            }
        }
        Ok(())
    }
}

fn priority(op: char) -> i32 {
    match op {
        '=' => 1,
        '+' | '-' => 2,
        '*' | '/' => 3,
        _ => -1,
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

fn emit_expr(a: Ast) {
    match a {
        OpInt(c, box l, box r) => {
            if c == '=' {
                emit_expr(r);
                if let Sym(_, pos) = l {
                    println!("\tmov %eax, -{}(%rbp)", pos * 4);
                } else {
                    panic!("Synbol expected");
                }
                return;
            }
            emit_expr(l);
            println!("\tpush %rax");
            emit_expr(r);
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
        Sym(_, pos) => {
            println!("\tmov -{}(%rbp), %eax", pos * 4);
        }
    }
}

struct R {
    buf: Vec<u8>,
    vars: Vec<Var>,
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

    fn find_var(&self, name: &str) -> Option<Var> {
        for v in &self.vars {
            if v.name == name {
                return Some(v.clone());
            }
        }
        None
    }

    fn make_var(&mut self, name: String) -> Var {
        let v = Var {
            name: name,
            pos: self.vars.len() + 1,
        };
        self.vars.push(v.clone());
        v
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

    fn read_symbol(&mut self, c: char) -> Ast {
        let mut name = String::new();
        name.push(c);
        loop {
            let c = self.getc().expect("Unexpected EOF");
            if !c.is_alphabetic() {
                self.ungetc();
                break;
            }
            name.push(c);
            if name.len() >= BUFLEN {
                panic!("Symbol too long");
            }
        }
        let v = match self.find_var(&name) {
            None => self.make_var(name),
            Some(v) => v,
        };
        Sym(v.name, v.pos)
    }

    fn read_prim(&mut self) -> Option<Ast> {
        match self.getc() {
            None => None,
            Some(c) => {
                if let Some(k) = c.to_digit(10) {
                    Some(self.read_number(k as i32))
                } else if c.is_alphabetic() {
                    Some(self.read_symbol(c))
                } else {
                    panic!("Don't know how to handle '{}'", c);
                }
            }
        }
    }

    fn read_expr2(&mut self, prec: i32) -> Option<Ast> {
        self.skip_space();
        let mut ast = self.read_prim();
        loop {
            self.skip_space();
            match self.getc() {
                None => {
                    return ast;
                }
                Some(c) => {
                    let prec2 = priority(c);
                    if prec2 < 0 || prec2 < prec {
                        self.ungetc();
                        return ast;
                    }
                    self.skip_space();
                    ast = Some(OpInt(c,
                                     Box::new(ast.unwrap()),
                                     Box::new(self.read_expr2(prec2 + 1).unwrap())));
                }
            }
        }
    }

    fn read_expr(&mut self) -> Option<Ast> {
        let r = self.read_expr2(0);
        if r.is_none() {
            return None;
        }
        self.skip_space();
        let c = self.getc().expect("Unexpected EOF");
        if c != ';' {
            panic!("Unterminated expression");
        }
        r
    }
}

fn main() {
    let wantast = std::env::args().nth(1) == Some(String::from("-a"));

    let mut buf = vec![];
    std::io::stdin().read_to_end(&mut buf).unwrap();
    let mut r = R {
        buf: buf,
        vars: vec![],
        p: 0,
    };

    if !wantast {
        println!(".text");
        println!("\t.global mymain");
        println!("\tmymain:");
    }

    while let Some(ast) = r.read_expr() {
        if wantast {
            print!("{}", ast);
        } else {
            emit_expr(ast);
        }
    }
    if !wantast {
        println!("\tret");
    }
}
