//! Regexp compiler for x86.

use std::{env, error::Error};

fn main() -> Result<(), Box<dyn Error>> {
    let args: Vec<String> = env::args().collect();
    if args.len() != 3 {
        return Err("usage: regex86 regexp string".into());
    }
    let re = &args[1];
    let s = &args[2];

    let tokens = regex86::scan(re);
    let expr = regex86::parse(&tokens).map_err(|_| "could not parse regexp")?;
    let mut emu = regex86::Emulator::new(&expr);

    if !emu.emulate(s) {
        return Err("no match".into());
    }

    Ok(())
}
