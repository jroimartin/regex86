//! Regexp compiler for x86.

use regex86::Regexp;

fn main() {
    let re = Regexp::parse(&Regexp::scan("a+(b?|c)*d")).unwrap();
    println!("{re}");
}
