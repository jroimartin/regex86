//! Regexp compiler for x86.

use std::{env, process};

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() != 3 {
        eprintln!("usage: regex86 regexp string");
        process::exit(2);
    }
    let re = &args[1];
    let s = &args[2];

    let tokens = regex86::scan(re);
    let expr = regex86::parse(&tokens).expect("could not parse regexp");
    let mut emu = regex86::Emulator::new(&expr);

    if emu.emulate(s) {
        process::exit(0)
    } else {
        process::exit(1)
    };
}
