use std::io::Read;

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

    fn compile_number(&mut self, mut n: i8) {
        while let Some(c) = self.getc() {
            if c.is_whitespace() {
                break;
            } else if !c.is_digit(10) {
                panic!("Invalid character in number: '{}'", c);
            } else {
                n = n * 10 + (c as i8 - '0' as i8);
            }
        }
        println!(".text");
        println!("\t.global intfn");
        println!("intfn:");
        println!("\tmov ${}, %rax", n);
        println!("\tret");
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
                return;
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
        if c.is_digit(10) {
            return self.compile_number(c as i8 - '0' as i8);
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
