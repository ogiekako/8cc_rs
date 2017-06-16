#![feature(box_syntax, box_patterns)]

use std::io::Read;
extern crate r8cc;
use r8cc::lex::{Lex, Token};

const EXPR_LEN: usize = 100;
const MAX_ARGS: usize = 6;
const REGS: &'static [&'static str] = &["rdi", "rsi", "rdx", "rcx", "r8", "r9"];

#[derive(Clone, Copy)]
enum CType {
    Void,
    Int,
    Char,
    Str,
}

#[derive(Clone)]
enum Node {
    OpInt(char, Box<Ast>, Box<Ast>),
    Char(char),
    Int(i32),
    // name, pos
    Var(CType, String, usize),
    // sval, sid
    Str(String, usize),
    // name, args
    Funcall(String, Vec<Ast>),
    // Var, expr
    Decl(Box<Ast>, Box<Ast>),
}

#[derive(Clone)]
struct Ast {
    ctype: CType,
    node: Node,
}

fn make_ast_op(op: char, ctype: CType, left: Ast, right: Ast) -> Ast {
    Ast {
        ctype: ctype,
        node: OpInt(op, Box::new(left), Box::new(right)),
    }
}

fn make_ast_int(val: i32) -> Ast {
    Ast {
        ctype: CType::Int,
        node: Int(val),
    }
}

fn make_ast_char(c: char) -> Ast {
    Ast {
        ctype: CType::Char,
        node: Char(c),
    }
}

fn make_ast_funcall(name: String, args: Vec<Ast>) -> Ast {
    Ast {
        ctype: CType::Int,
        node: Funcall(name, args),
    }
}

use Node::*;

fn ensure_lvalue(ast: &Ast) {
    if let &Var(_, _, _) = &ast.node {
    } else {
        panic!("variable expected");
    }
}

fn result_type(op: char, a: &Ast, b: &Ast) -> CType {
    let err = || {
        panic!("incompatible operands: {} and {} for {}", a, b, op);
    };
    match (a.ctype, b.ctype) {
        (CType::Void, _) | (_, CType::Void) => err(),
        (CType::Int, CType::Int) |
        (CType::Int, CType::Char) |
        (CType::Char, CType::Int) |
        (CType::Char, CType::Char) => CType::Int,
        _ => err(),
    }
}

#[inline]
fn get_ctype(tok: &Token) -> Option<CType> {
    if let &Token::Ident(ref s) = tok {
        match s as &str {
            "int" => Some(CType::Int),
            "char" => Some(CType::Char),
            "string" => Some(CType::Str),
            _ => None,
        }
    } else {
        None
    }
}

#[inline]
fn is_type_keyword(tok: &Token) -> bool {
    get_ctype(tok).is_some()
}

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

impl std::fmt::Display for CType {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> Result<(), std::fmt::Error> {
        write!(f,
               "{}",
               match *self {
                   CType::Void => "void",
                   CType::Int => "int",
                   CType::Char => "char",
                   CType::Str => "string",
               })
    }
}

impl std::fmt::Display for Ast {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> Result<(), std::fmt::Error> {
        match &self.node {
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
            &Var(_, ref name, _) => {
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
            &Decl(ref var, ref init) => {
                match &var.as_ref().node {
                    &Var(ref ctype, ref name, _) => {
                        write!(f, "(decl {} {} {})", ctype, name, init.as_ref())?
                    }
                    _ => panic!("Internal error: unexpected var: {}", var),
                }
            }
        }
        Ok(())
    }
}

#[inline]
fn is_right_assoc(op: char) -> bool {
    op == '='
}

#[inline]
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

fn emit_assign(var: &Ast, value: &Ast) {
    emit_expr(value);
    if let &Var(_, _, pos) = &var.node {
        println!("\tmov %eax, -{}(%rbp)", pos * 4);
    } else {
        panic!("Synbol expected");
    }
}

fn emit_expr(a: &Ast) {
    match &a.node {
        &OpInt(c, ref l, ref r) => {
            // emit_binop
            if c == '=' {
                emit_assign(l, r);
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
        &Var(_, _, pos) => {
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
        &Decl(ref var, ref init) => {
            emit_assign(var, init);
        }
    }
}

struct R {
    lex: Lex,
    vars: Vec<Ast>,
    strings: Vec<Ast>,
}

impl R {
    fn find_var(&self, name: &str) -> Option<Ast> {
        for v in &self.vars {
            match &v.node {
                &Var(_, ref s, _) => {
                    if s == name {
                        return Some(v.clone());
                    }
                }
                _ => {
                    panic!("Internal error: expected Var but got {}", v);
                }
            };
        }
        None
    }

    fn make_ast_var(&mut self, ctype: CType, name: String) -> Ast {
        let p = self.vars.len() + 1;
        let var = Ast {
            ctype: ctype,
            node: Var(ctype, name, p),
        };
        self.vars.push(var.clone());
        var
    }

    fn make_ast_string(&mut self, name: String) -> Ast {
        let sid = self.strings.len();
        let r = Ast {
            ctype: CType::Str,
            node: Str(name, sid),
        };
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
            args.push(self.read_expr(0).unwrap());
            match self.lex.read_token() {
                Some(Token::Punct(')')) => break,
                Some(Token::Punct(',')) => {}
                other => panic!("Unexpected token: '{}'", other.unwrap()),
            }
        }
        if args.len() > MAX_ARGS {
            panic!("Too many arguments: {}", fname);
        }
        make_ast_funcall(fname, args)
    }

    fn read_ident_or_func(&mut self, name: String) -> Ast {
        let tok = self.lex.read_token();
        if let &Some(Token::Punct('(')) = &tok {
            return self.read_func_args(name);
        }
        if let Some(tok) = tok {
            self.lex.unget_token(tok);
        }
        self.find_var(&name).unwrap_or_else(|| panic!("Underfined variable: {}", name))
    }

    fn read_prim(&mut self) -> Option<Ast> {
        self.lex.read_token().map(|tok| match tok {
            Token::Ident(s) => self.read_ident_or_func(s.clone()),
            Token::Int(i) => make_ast_int(i),
            Token::Char(c) => make_ast_char(c),
            Token::Str(s) => self.make_ast_string(s),
            Token::Punct(c) => panic!("unexpected character: '{}'", c),

            Token::Eof => panic!("Internal error: Eof should not appear."),
        })
    }

    fn read_expr(&mut self, prec: i32) -> Option<Ast> {
        let ast = self.read_prim();
        if ast.is_none() {
            return None;
        }
        let mut ast = ast.unwrap();
        loop {
            let tok = self.lex.read_token();
            if let &Some(Token::Punct(c)) = &tok {
                let prec2 = priority(c);
                if prec2 >= prec {
                    if tok == Some(Token::Punct('=')) {
                        ensure_lvalue(&ast);
                    }
                    let rest = self.read_expr(prec2 + if is_right_assoc(c) { 0 } else { 1 })
                        .unwrap();
                    let ctype = result_type(c, &ast, &rest);
                    ast = make_ast_op(c, ctype, ast, rest);
                    continue;
                }
            }
            if let Some(tok) = tok {
                self.lex.unget_token(tok);
            }
            return Some(ast);
        }
    }

    fn expect(&mut self, punct: char) {
        let tok = self.lex.read_token();
        if Some(Token::Punct(punct)) != tok {
            panic!("'{}' expected, but got {}",
                   punct,
                   tok.unwrap_or(Token::Eof));
        }
    }

    fn read_decl(&mut self) -> Ast {
        let ctype = get_ctype(&self.lex.read_token().expect("internal error: ctype expected"))
            .expect("internal error: ctype expected");
        let name = self.lex.read_token();

        let var;
        if let Some(Token::Ident(s)) = name {
            var = self.make_ast_var(ctype, s);
        } else {
            panic!("Identifier expected, but got {}",
                   name.unwrap_or(Token::Eof));
        }
        self.expect('=');
        let init = self.read_expr(0).expect("Unexpected EOF");
        Ast {
            ctype: ctype,
            node: Decl(Box::new(var), Box::new(init)),
        }
    }

    fn read_decl_or_stmt(&mut self) -> Option<Ast> {
        let tok = self.lex.peek_token();
        tok.map(|tok| {
            let r = if is_type_keyword(&tok) {
                self.read_decl()
            } else {
                self.read_expr(0).expect("Unexpected EOF")
            };
            self.expect(';');
            r
        })
    }

    fn emit_data_section(&self) {
        if self.strings.is_empty() {
            return;
        }
        println!(".data");
        for p in &self.strings {
            if let &Str(ref sval, sid) = &p.node {
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
        if let Some(t) = r.read_decl_or_stmt() {
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
