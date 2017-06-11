#![feature(box_syntax, box_patterns)]

use std::io::Read;

const BUFLEN: usize = 256;
const EXPR_LEN: usize = 100;
const MAX_ARGS: usize = 6;
const REGS: &'static [&'static str] = &["rdi", "rsi", "rdx", "rcx", "r8", "r9"];

#[derive(Debug, Clone)]
enum Ast {
    OpInt(char, Box<Ast>, Box<Ast>),
    Char(char),
    Int(i32),
    // name, pos
    Var(String, usize),
    // sval, sid
    Str(String, usize),
    // name, args
    Funcall(String, Vec<Ast>),
}

use Ast::*;

#[derive(Clone)]
struct V {
    name: String,
    pos: usize,
}

struct Quote<'a> {
    s: &'a str,
}

fn quote(s: &str) -> Quote {
    Quote { s: s }
}

impl<'a> std::fmt::Display for Quote<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> Result<(), std::fmt::Error> {
        for c in self.s.chars() {
            if c == '\"' || c == '\\' {
                write!(f, "\\")?;
            }
            write!(f, "{}", c)?;
        }
        Ok(())
    }
}

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
            &Char(c) => {
                write!(f, "'{}'", c)?;
            }
            &Var(ref name, _) => {
                write!(f, "{}", name)?;
            }
            &Str(ref name, _) => {
                write!(f, "\"")?;
                write!(f, "{}", quote(name))?;
                write!(f, "\"")?;
            }
            &Funcall(ref name, ref args) => {
                write!(f, "{}(", name)?;
                for i in 0..args.len() {
                    if i > 0 {
                        write!(f, ",")?;
                    }
                    args[i].fmt(f)?;
                }
                write!(f, ")")?;
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

fn emit_expr(a: &Ast) {
    match a {
        &OpInt(c, ref l, ref r) => {
            if c == '=' {
                emit_expr(r);
                if let &Var(_, pos) = l.as_ref() {
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
        &Int(n) => {
            println!("\tmov ${}, %eax", n);
        }
        &Char(c) => {
            println!("\tmov ${}, %eax", c as u32);
        }
        &Var(_, pos) => {
            println!("\tmov -{}(%rbp), %eax", pos * 4);
        }
        &Str(_, sid) => {
            println!("\tlea .s{}(%rip), %rax", sid);
        }
        &Funcall(ref name, ref args) => {
            for i in 1..args.len() {
                println!("\tpush %{}", REGS[i]);
            }
            for arg in args.iter() {
                emit_expr(arg);
                println!("\tpush %rax");
            }
            for i in (0..args.len()).rev() {
                println!("\tpop %{}", REGS[i]);
            }
            println!("\tmov $0, %eax");
            println!("\tcall {}", name);
            for i in (1..args.len()).rev() {
                println!("\tpop %{}", REGS[i]);
            }
        }
    }
}

struct R {
    buf: Vec<u8>,
    vars: Vec<V>,
    strings: Vec<Ast>,
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

    fn find_var(&self, name: &str) -> Option<V> {
        for v in &self.vars {
            if v.name == name {
                return Some(v.clone());
            }
        }
        None
    }

    fn make_var(&mut self, name: String) -> V {
        let v = V {
            name: name,
            pos: self.vars.len() + 1,
        };
        self.vars.push(v.clone());
        v
    }

    fn make_ast_str(&mut self, name: String) -> Ast {
        let sid = self.strings.len();
        let r = Str(name, sid);
        self.strings.push(r.clone());
        r
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

    fn read_ident(&mut self, c: char) -> String {
        let mut name = String::new();
        name.push(c);
        loop {
            let c = self.getc().expect("Unexpected EOF");
            if !c.is_alphanumeric() {
                self.ungetc();
                return name;
            }
            name.push(c);
            if name.len() >= BUFLEN {
                panic!("Identifier too long");
            }
        }
    }

    fn read_func_args(&mut self, fname: String) -> Ast {
        let mut args = vec![];
        for _ in 0..MAX_ARGS + 1 {
            self.skip_space();
            let c = self.getc().expect("Unexpected EOF");
            if c == ')' {
                break;
            }
            self.ungetc();
            args.push(self.read_expr2(0).unwrap());
            match self.getc() {
                Some(')') => break,
                Some(',') => self.skip_space(),
                _ => panic!("Unexpected character: '{}'", c),
            }
        }
        if args.len() == MAX_ARGS + 1 {
            panic!("Too many arguments: {}", fname);
        }
        Funcall(fname, args)
    }

    fn read_ident_or_func(&mut self, c: char) -> Ast {
        let name = self.read_ident(c);
        self.skip_space();
        match self.getc() {
            Some('(') => self.read_func_args(name),
            _ => {
                self.ungetc();
                let v = self.find_var(&name).unwrap_or_else(|| self.make_var(name));
                Var(v.name, v.pos)
            }
        }
    }

    fn read_char(&mut self) -> Ast {
        let err = || panic!("Unterminated char");
        let mut c = self.getc().unwrap_or_else(&err);
        if c == '\\' {
            c = self.getc().unwrap_or_else(&err);
        }
        let c2 = self.getc().unwrap_or_else(&err);
        if c2 != '\'' {
            panic!("Malformed char constant");
        }
        Char(c)
    }

    fn read_prim(&mut self) -> Option<Ast> {
        match self.getc() {
            None => None,
            Some(c) => {
                if let Some(k) = c.to_digit(10) {
                    Some(self.read_number(k as i32))
                } else if c == '"' {
                    Some(self.read_string())
                } else if c == '\'' {
                    Some(self.read_char())
                } else if c.is_alphabetic() {
                    Some(self.read_ident_or_func(c))
                } else {
                    panic!("Don't know how to handle '{}'", c);
                }
            }
        }
    }

    fn read_string(&mut self) -> Ast {
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
        self.make_ast_str(buf)
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
            panic!("Unterminated expression: {}", c);
        }
        r
    }

    fn emit_data_section(&self) {
        if self.strings.is_empty() {
            return;
        }
        println!(".data");
        for p in &self.strings {
            if let &Str(ref sval, sid) = p {
                println!(".s{}:", sid);
                print!(".string \"{}", quote(sval));
                println!("\"");
            } else {
                panic!("Str expected but was {}", p);
            }
        }
    }
}


fn main() {
    let wantast = std::env::args().nth(1) == Some(String::from("-a"));

    let mut buf = vec![];
    std::io::stdin().read_to_end(&mut buf).unwrap();
    let mut r = R {
        buf: buf,
        vars: vec![],
        strings: vec![],
        p: 0,
    };

    let mut exprs = vec![];
    for _ in 0..EXPR_LEN {
        if let Some(t) = r.read_expr() {
            exprs.push(t);
        } else {
            break;
        }
    }

    if !wantast {
        r.emit_data_section();
        println!(".text");
        println!("\t.global mymain");
        println!("\tmymain:");
    }

    for expr in exprs.iter() {
        if wantast {
            print!("{}", expr);
        } else {
            emit_expr(&expr);
        }
    }
    if !wantast {
        println!("\tret");
    }
}
