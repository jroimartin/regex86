//! Regexp compiler for x86.

use std::fmt;

use clap::{Parser, Subcommand};
use regex86::{CompilationError, Compiler, ParsingError, Regexp};

/// CLI error.
enum CliError {
    /// Generic error.
    Str(String),

    /// Parsing error.
    Parsing(ParsingError),

    /// Compilation error.
    Compilation(CompilationError),
}

impl From<&str> for CliError {
    fn from(err: &str) -> CliError {
        CliError::Str(String::from(err))
    }
}

impl From<ParsingError> for CliError {
    fn from(err: ParsingError) -> CliError {
        CliError::Parsing(err)
    }
}

impl From<CompilationError> for CliError {
    fn from(err: CompilationError) -> CliError {
        CliError::Compilation(err)
    }
}

impl fmt::Display for CliError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            CliError::Str(s) => write!(f, "{s}"),
            CliError::Parsing(err) => write!(f, "parsing error: {err}"),
            CliError::Compilation(err) => write!(f, "compilation error: {err}"),
        }
    }
}

impl fmt::Debug for CliError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
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
            let mut re = Regexp::from_regexp(&regexp)?;
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
