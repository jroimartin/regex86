//! Regexp compiler for x86.

use std::{env, error::Error};

fn main() -> Result<(), Box<dyn Error>> {
    let args: Vec<String> = env::args().collect();
    if args.len() != 3 {
        return Err("usage: regex86 regexp string".into());
    }
    let re = &args[1];
    let s = &args[2];

    let mut emu = regex86::Emulator::from_regexp(re).map_err(|_| "parsing error")?;
    if !emu.emulate(s) {
        return Err("no match".into());
    }
    Ok(())
}
