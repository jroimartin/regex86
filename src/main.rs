//! Regexp compiler for x86.

fn main() {
    let tokens = regex86::scan("a+(b?|c)*d");
    let ast = regex86::parse(&tokens).expect("parse regexp");
    println!("{ast}");
}
