//! Regexp compiler library for x86.
//!
//! The only supported metacharacters are `* + ? | ( )`.  Their
//! precedence, from weakest to strongest binding, is first
//! alternation (`|`), then concatenation, and finally the repetition
//! operators (`*`, `+` and `?`).

use std::{fmt, iter};

/// Parsing error.
#[derive(Debug, Eq, PartialEq)]
pub enum ParsingError {
    UnmatchedParenthesis,
    UnexpectedToken(Token),
    Eof,
}

/// Lexical token.
#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub enum Token {
    Concat,
    Star,
    Plus,
    Question,
    Pipe,
    LeftParen,
    RightParen,
    Char(char),
}

impl From<char> for Token {
    fn from(c: char) -> Token {
        match c {
            '*' => Token::Star,
            '+' => Token::Plus,
            '?' => Token::Question,
            '|' => Token::Pipe,
            '(' => Token::LeftParen,
            ')' => Token::RightParen,
            _ => Token::Char(c),
        }
    }
}

/// The Abstract Syntax Tree (AST) of a regular expression.
///
/// Implements following grammar (from weakest to strongest binding):
///
/// alternation   = concatenation {PIPE concatenation} .
/// concatenation = repetition {CONCAT repetition} .
/// repetition    = grouping [STAR | PLUS | QUESTION] .
/// grouping      = LEFT_PAREN alternation RIGHT_PARENT | matching .
/// matching      = CHAR .
///
/// The alternation and concatenation operators are left-associative.
#[derive(Debug, Eq, PartialEq)]
pub enum Expr {
    Alternation { lhs: Box<Expr>, rhs: Box<Expr> },
    Concatenation { lhs: Box<Expr>, rhs: Box<Expr> },
    Repetition(Box<Expr>, Times),
    Grouping(Box<Expr>),
    Matching(char),
}

/// Number of times the inner expression of an [`Expr::Repetition`]
/// expression is repeated.
#[derive(Debug, Eq, PartialEq)]
pub enum Times {
    ZeroOrMore,
    OneOrMore,
    ZeroOrOne,
}

impl fmt::Display for Times {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Times::ZeroOrMore => write!(f, "*"),
            Times::OneOrMore => write!(f, "+"),
            Times::ZeroOrOne => write!(f, "?"),
        }
    }
}

impl fmt::Display for Expr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Expr::Alternation { lhs, rhs } => write!(f, "{lhs}|{rhs}"),
            Expr::Concatenation { lhs, rhs } => write!(f, "{lhs}{rhs}"),
            Expr::Repetition(expr, op) => write!(f, "{expr}{op}"),
            Expr::Grouping(expr) => write!(f, "({expr})"),
            Expr::Matching(ch) => write!(f, "{ch}"),
        }
    }
}

impl Expr {
    /// Parses an expression from the provided token iterator.
    fn parse<I: Iterator<Item = Token>>(
        tokens: &mut iter::Peekable<I>,
    ) -> Result<Expr, ParsingError> {
        let expr = Expr::parse_alternation(tokens)?;
        match tokens.next() {
            Some(tok) => Err(ParsingError::UnexpectedToken(tok)),
            None => Ok(expr),
        }
    }

    /// Parses an alternation expression from the provided token
    /// iterator.
    fn parse_alternation<I: Iterator<Item = Token>>(
        tokens: &mut iter::Peekable<I>,
    ) -> Result<Expr, ParsingError> {
        let mut expr = Expr::parse_concatenation(tokens)?;
        while Expr::match_token(tokens, Token::Pipe) {
            let rhs = Expr::parse_concatenation(tokens)?;
            expr = Expr::Alternation {
                lhs: Box::new(expr),
                rhs: Box::new(rhs),
            };
        }
        Ok(expr)
    }

    /// Parses a concatenation expression from the provided token
    /// iterator.
    fn parse_concatenation<I: Iterator<Item = Token>>(
        tokens: &mut iter::Peekable<I>,
    ) -> Result<Expr, ParsingError> {
        let mut expr = Expr::parse_repetition(tokens)?;
        while Expr::match_token(tokens, Token::Concat) {
            let rhs = Expr::parse_repetition(tokens)?;
            expr = Expr::Concatenation {
                lhs: Box::new(expr),
                rhs: Box::new(rhs),
            }
        }
        Ok(expr)
    }

    /// Parses a repetition expression from the provided token
    /// iterator.
    fn parse_repetition<I: Iterator<Item = Token>>(
        tokens: &mut iter::Peekable<I>,
    ) -> Result<Expr, ParsingError> {
        let expr = Expr::parse_grouping(tokens)?;
        if Expr::match_token(tokens, Token::Star) {
            return Ok(Expr::Repetition(Box::new(expr), Times::ZeroOrMore));
        }
        if Expr::match_token(tokens, Token::Plus) {
            return Ok(Expr::Repetition(Box::new(expr), Times::OneOrMore));
        }
        if Expr::match_token(tokens, Token::Question) {
            return Ok(Expr::Repetition(Box::new(expr), Times::ZeroOrOne));
        }
        Ok(expr)
    }

    /// Parses a grouping expression from the provided token iterator.
    fn parse_grouping<I: Iterator<Item = Token>>(
        tokens: &mut iter::Peekable<I>,
    ) -> Result<Expr, ParsingError> {
        if Expr::match_token(tokens, Token::LeftParen) {
            let expr = Expr::parse_alternation(tokens)?;
            return match tokens.next() {
                Some(Token::RightParen) => Ok(Expr::Grouping(Box::new(expr))),
                _ => Err(ParsingError::UnmatchedParenthesis),
            };
        }
        Expr::parse_matching(tokens)
    }

    /// Parses a matching expression from the provided token iterator.
    fn parse_matching<I: Iterator<Item = Token>>(
        tokens: &mut iter::Peekable<I>,
    ) -> Result<Expr, ParsingError> {
        match tokens.next() {
            Some(Token::Char(ch)) => Ok(Expr::Matching(ch)),
            Some(tok) => Err(ParsingError::UnexpectedToken(tok)),
            None => Err(ParsingError::Eof),
        }
    }

    /// Matches the next token in the provided token iterator against
    /// the passed token.  In the case of a match, it returns true and
    /// advances the iterator cursor.
    fn match_token<I: Iterator<Item = Token>>(tokens: &mut iter::Peekable<I>, tok: Token) -> bool {
        match tokens.peek() {
            Some(&t) if t == tok => {
                tokens.next();
                true
            }
            _ => false,
        }
    }
}

/// Represents a regular expression.
pub struct Regexp;

impl Regexp {
    /// Performs the lexical analysis of the provided regular
    /// expression.
    pub fn scan(regexp: &str) -> Vec<Token> {
        let mut tokens = Vec::new();
        let mut chars = regexp.chars().peekable();
        while let Some(cur) = chars.next() {
            let cur = cur.into();
            tokens.push(cur);
            if let Some(&next) = chars.peek() {
                let next = next.into();
                if matches!(
                    cur,
                    Token::Char(_)
                        | Token::Star
                        | Token::Plus
                        | Token::Question
                        | Token::RightParen
                ) && matches!(next, Token::Char(_) | Token::LeftParen)
                {
                    tokens.push(Token::Concat);
                }
            }
        }
        tokens
    }

    /// Performs the syntactic analysis the provided token stream.  It
    /// returns the parsed AST.
    pub fn parse(tokens: &[Token]) -> Result<Expr, ParsingError> {
        let mut tokens = tokens.iter().copied().peekable();
        Expr::parse(&mut tokens)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn scan_empty() {
        assert_eq!(Regexp::scan(""), &[]);
    }

    #[test]
    fn scan_not_quantifiable() {
        assert_eq!(
            Regexp::scan("a++"),
            &[Token::Char('a'), Token::Plus, Token::Plus]
        );
    }

    #[test]
    fn scan_complex() {
        assert_eq!(
            Regexp::scan("a+(bc?|d+e+)*f"),
            &[
                Token::Char('a'),
                Token::Plus,
                Token::Concat,
                Token::LeftParen,
                Token::Char('b'),
                Token::Concat,
                Token::Char('c'),
                Token::Question,
                Token::Pipe,
                Token::Char('d'),
                Token::Plus,
                Token::Concat,
                Token::Char('e'),
                Token::Plus,
                Token::RightParen,
                Token::Star,
                Token::Concat,
                Token::Char('f'),
            ]
        );
    }

    #[test]
    fn parse_char() {
        assert_eq!(Regexp::parse(&Regexp::scan("a")), Ok(Expr::Matching('a')));
    }

    #[test]
    fn parse_concat() {
        assert_eq!(
            Regexp::parse(&Regexp::scan("ab")),
            Ok(Expr::Concatenation {
                lhs: Box::new(Expr::Matching('a')),
                rhs: Box::new(Expr::Matching('b')),
            })
        );
    }

    #[test]
    fn parse_repetition() {
        assert_eq!(
            Regexp::parse(&Regexp::scan("a*b")),
            Ok(Expr::Concatenation {
                lhs: Box::new(Expr::Repetition(
                    Box::new(Expr::Matching('a')),
                    Times::ZeroOrMore
                )),
                rhs: Box::new(Expr::Matching('b')),
            })
        );
    }

    #[test]
    fn parse_alternation() {
        assert_eq!(
            Regexp::parse(&Regexp::scan("a|b|c")),
            Ok(Expr::Alternation {
                lhs: Box::new(Expr::Alternation {
                    lhs: Box::new(Expr::Matching('a')),
                    rhs: Box::new(Expr::Matching('b')),
                }),
                rhs: Box::new(Expr::Matching('c')),
            })
        )
    }

    #[test]
    fn parse_groups() {
        assert_eq!(
            Regexp::parse(&Regexp::scan("a|((b|c)|d)")),
            Ok(Expr::Alternation {
                lhs: Box::new(Expr::Matching('a')),
                rhs: Box::new(Expr::Grouping(Box::new(Expr::Alternation {
                    lhs: Box::new(Expr::Grouping(Box::new(Expr::Alternation {
                        lhs: Box::new(Expr::Matching('b')),
                        rhs: Box::new(Expr::Matching('c')),
                    }))),
                    rhs: Box::new(Expr::Matching('d')),
                }))),
            })
        )
    }

    #[test]
    fn parse_complex() {
        assert_eq!(
            Regexp::parse(&Regexp::scan("a+(b?|c)*d")),
            Ok(Expr::Concatenation {
                lhs: Box::new(Expr::Concatenation {
                    lhs: Box::new(Expr::Repetition(
                        Box::new(Expr::Matching('a')),
                        Times::OneOrMore
                    )),
                    rhs: Box::new(Expr::Repetition(
                        Box::new(Expr::Grouping(Box::new(Expr::Alternation {
                            lhs: Box::new(Expr::Repetition(
                                Box::new(Expr::Matching('b')),
                                Times::ZeroOrOne
                            )),
                            rhs: Box::new(Expr::Matching('c')),
                        }))),
                        Times::ZeroOrMore
                    )),
                }),
                rhs: Box::new(Expr::Matching('d')),
            })
        )
    }

    #[test]
    fn parse_error_unmatched_parenthesis() {
        assert_eq!(
            Regexp::parse(&Regexp::scan("((a|b)|c")),
            Err(ParsingError::UnmatchedParenthesis)
        )
    }

    #[test]
    fn parse_error_not_quantifiable() {
        assert_eq!(
            Regexp::parse(&Regexp::scan("a++")),
            Err(ParsingError::UnexpectedToken(Token::Plus))
        )
    }

    #[test]
    fn parse_error_unexpected_token() {
        assert_eq!(
            Regexp::parse(&Regexp::scan("|a")),
            Err(ParsingError::UnexpectedToken(Token::Pipe))
        )
    }

    #[test]
    fn parse_error_eof() {
        assert_eq!(Regexp::parse(&Regexp::scan("a|")), Err(ParsingError::Eof))
    }

    #[test]
    fn ast_to_string() {
        assert_eq!(
            Regexp::parse(&Regexp::scan("a+(b?|c)*d"))
                .unwrap()
                .to_string(),
            "a+(b?|c)*d"
        )
    }
}
