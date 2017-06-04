extern crate contest;
use contest::scanner;
fn main() {
    let mut sc = scanner::new(std::io::stdin());

    let val: i32 = sc.next().unwrap();

    print!("\t.text\n\t.global mymain\nmymain:\n\tmov ${}, %eax\n\tret\n",
           val);
}
