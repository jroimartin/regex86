//! Regexp compiler for x86.

use std::{
    fmt::{self, Debug, Display, Formatter},
    str::FromStr,
};

use clap::{Parser, Subcommand};
use regex86::{self, Compiler, Regexp};

/// CLI error.
enum CliError {
    /// Generic error.
    Str(String),

    /// Regular expression error.
    Regexp(regex86::Error),
}

impl From<&str> for CliError {
    fn from(err: &str) -> CliError {
        CliError::Str(String::from(err))
    }
}

impl From<regex86::Error> for CliError {
    fn from(err: regex86::Error) -> CliError {
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

/// Regexp compiler for x86.
#[derive(Parser)]
struct Cli {
    /// A subcommand of the CLI.
    #[command(subcommand)]
    command: Command,
}

/// Represents a subcommand of the CLI.
#[derive(Subcommand)]
enum Command {
    /// Reports whether the regular expression matches the string.
    Match {
        /// Regular expression.
        regexp: String,

        /// Text to match.
        text: String,
    },

    /// Compiles a regular expression into x86 assembly.
    Compile {
        /// Regular expression.
        regexp: String,
    },
}

fn main() -> Result<(), CliError> {
    let cli = Cli::parse();
    match cli.command {
        Command::Match { regexp, text } => {
            let mut re = Regexp::from_str(&regexp)?;
            if !re.matches(&text) {
                return Err("no match".into());
            }
        }
        Command::Compile { regexp } => {
            let code = Compiler::compile_regexp(&regexp)?;
            print!("{code}");
        }
    }
    Ok(())
}
