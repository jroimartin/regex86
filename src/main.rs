//! Command line interface for the `regexp` crate.

use std::{
    env,
    fmt::{self, Debug, Display, Formatter},
    str::FromStr,
};

use regexp::{self, Regexp};

/// CLI error.
enum CliError {
    /// Generic error.
    Str(String),

    /// Regular expression error.
    Regexp(regexp::Error),
}

impl From<&str> for CliError {
    fn from(err: &str) -> CliError {
        CliError::Str(String::from(err))
    }
}

impl From<regexp::Error> for CliError {
    fn from(err: regexp::Error) -> CliError {
        CliError::Regexp(err)
    }
}

impl Display for CliError {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            CliError::Str(s) => write!(f, "{s}"),
            CliError::Regexp(err) => write!(f, "regular expression error: {err}"),
        }
    }
}

impl Debug for CliError {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{self}")
    }
}

fn main() -> Result<(), CliError> {
    let args: Vec<String> = env::args().collect();
    if args.len() != 3 {
        return Err("usage: regexp <regexp> <text>".into());
    }
    let sre = &args[1];
    let text = &args[2];

    let mut re = Regexp::from_str(sre)?;
    if !re.matches(text) {
        return Err("no match".into());
    }
    Ok(())
}
