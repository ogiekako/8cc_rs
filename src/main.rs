#![feature(box_syntax, box_patterns)]

use std::io::Read;
extern crate r8cc;
use r8cc::lex::{Lex, Token};

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
    lex: Lex,
    vars: Vec<V>,
    strings: Vec<Ast>,
}

impl R {
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

    fn make_ast_string(&mut self, name: String) -> Ast {
        let sid = self.strings.len();
        let r = Str(name, sid);
        self.strings.push(r.clone());
        r
    }

    fn read_func_args(&mut self, fname: String) -> Ast {
        let mut args = vec![];
        for _ in 0..MAX_ARGS + 1 {
            let tok = self.lex.read_token();
            if Some(Token::Punct(')')) == tok {
                break;
            }
            if let Some(tok) = tok {
                self.lex.unget_token(tok);
            }
            args.push(self.read_expr2(0).unwrap());
            match self.lex.read_token() {
                Some(Token::Punct(')')) => break,
                Some(Token::Punct(',')) => {}
                other => panic!("Unexpected token: '{}'", other.unwrap()),
            }
        }
        if args.len() > MAX_ARGS {
            panic!("Too many arguments: {}", fname);
        }
        Funcall(fname, args)
    }

    fn read_ident_or_func(&mut self, name: String) -> Ast {
        let tok = self.lex.read_token();
        if let &Some(Token::Punct('(')) = &tok {
            return self.read_func_args(name);
        }
        if let Some(tok) = tok {
            self.lex.unget_token(tok);
        }
        let v = self.find_var(&name).unwrap_or_else(|| self.make_var(name));
        Var(v.name, v.pos)
    }

    fn read_prim(&mut self) -> Option<Ast> {
        self.lex.read_token().map(|tok| match tok {
            Token::Ident(s) => self.read_ident_or_func(s.clone()),
            Token::Int(i) => Ast::Int(i),
            Token::Char(c) => Ast::Char(c),
            Token::Str(s) => self.make_ast_string(s),
            Token::Punct(c) => panic!("unexpected character: '{}'", c),
        })
    }

    fn read_expr2(&mut self, prec: i32) -> Option<Ast> {
        let mut ast = self.read_prim();
        loop {
            let tok = self.lex.read_token();
            if let &Some(Token::Punct(c)) = &tok {
                let prec2 = priority(c);
                if prec2 >= prec {
                    ast = Some(OpInt(c,
                                     Box::new(ast.unwrap()),
                                     Box::new(self.read_expr2(prec2 + 1).unwrap())));
                    continue;
                }
            }
            if let Some(tok) = tok {
                self.lex.unget_token(tok);
            }
            return ast;
        }
    }

    fn read_expr(&mut self) -> Option<Ast> {
        let r = self.read_expr2(0);
        if r.is_none() {
            return None;
        }
        let tok = self.lex.read_token().expect("Unexpected EOF");
        if Token::Punct(';') != tok {
            panic!("Unterminated expression: {}", tok);
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
        lex: Lex::new(buf),
        vars: vec![],
        strings: vec![],
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
