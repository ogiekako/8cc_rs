use std::io::Read;
use std::process;

static BUFLEN: usize = 256;

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

    fn read_number(&mut self, mut n: i32) -> i32 {
        while let Some(c) = self.getc() {
            if let Some(k) = c.to_digit(10) {
                n = n * 10 + k as i32;
            } else {
                self.unget();
                return n;
            }
        }
        return n;
    }

    fn compile_expr2(&mut self) {
        loop {
            self.skip_space();
            match self.getc() {
                None => {
                    println!("\tret");
                    process::exit(0);
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
                    let c = self.getc().expect("Number expected but EOF");
                    if let Some(n) = c.to_digit(10) {
                        println!("\t{} ${}, %rax", op, self.read_number(n as i32));
                    } else {
                        panic!("Number expected, but got '{}'", c);
                    }
                }
            }
        }
    }

    fn compile_expr(&mut self, mut n: i32) {
        n = self.read_number(n);
        println!(".text");
        println!("\t.global intfn");
        println!("intfn:");
        println!("\tmov ${}, %rax", n);

        self.compile_expr2();
    }

    fn compile_string(&mut self) {
        let mut s = String::new();
        while let Some(c) = self.getc() {
            let mut c = c;
            if c == '"' {
                println!("\t.data");
                println!(".mydata:");
                println!(".string \"{}\"\n\t", s);
                println!("\t.text");
                println!("\t.global stringfn");
                println!("stringfn:");
                println!("lea .mydata(%rip), %rax");
                println!("ret");
                process::exit(0);
            } else if c == '\\' {
                c = self.getc().expect("Unterminated \\") as char;
            }
            s.push(c);
            if s.len() >= BUFLEN {
                panic!("String too long");
            }
        }
        panic!("Unterminated string");
    }

    fn compile(&mut self) {
        let c = self.getc().expect("No input");
        if let Some(k) = c.to_digit(10) {
            return self.compile_expr(k as i32);
        } else if c == '"' {
            return self.compile_string();
        } else {
            panic!("Don't know how to handle '{}'", c);
        }
    }
}

fn main() {
    let mut buf = vec![];
    std::io::stdin().read_to_end(&mut buf).unwrap();
    let mut r = R { buf: buf, p: 0 };

    r.compile();
}
