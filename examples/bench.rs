//! Benchmarks regex86.
//!
//! It measures the time to match the regular expression a?**n**a**n**
//! against the string a**n**.
//!
//! **n** denote string repetition, so that a?**3**a**3** is shorthand
//! for a?a?a?aaa.

use std::time::Instant;

use regex86::Regexp;

const SAMPLES: usize = 250;
const N: usize = 30;

fn main() {
    for n in 1..=N {
        let regexp = "a?".repeat(n) + &"a".repeat(n);
        let text = "a".repeat(n);
        let mut re = Regexp::from_regexp(&regexp).unwrap();

        let now = Instant::now();
        for _ in 0..SAMPLES {
            assert!(re.matches(&text));
        }
        let t = (now.elapsed().as_micros() as f64) / (SAMPLES as f64);

        println!("n = {n:2}    t = {t:6.2}us");
    }
}
