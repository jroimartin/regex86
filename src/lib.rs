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

/// Performs the lexical analysis of the provided regular expression.
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
                Token::Char(_) | Token::Star | Token::Plus | Token::Question | Token::RightParen
            ) && matches!(next, Token::Char(_) | Token::LeftParen)
            {
                tokens.push(Token::Concat);
            }
        }
    }
    tokens
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

/// Performs the syntactic analysis the provided tokens.  It returns
/// the parsed AST.
pub fn parse(tokens: &[Token]) -> Result<Expr, ParsingError> {
    Parser::new(tokens.iter().cloned()).parse()
}

/// Represents a syntactic token.
struct Parser<I: Iterator<Item = Token>> {
    /// Token peekable iterator.
    iter: iter::Peekable<I>,
}

impl<I: Iterator<Item = Token>> Parser<I> {
    /// Returns a new [`Parser`].
    fn new(tokens: I) -> Parser<I> {
        Parser {
            iter: tokens.peekable(),
        }
    }

    /// Parses an expression.
    fn parse(&mut self) -> Result<Expr, ParsingError> {
        let expr = self.parse_alternation()?;
        match self.iter.next() {
            Some(tok) => Err(ParsingError::UnexpectedToken(tok)),
            None => Ok(expr),
        }
    }

    /// Parses an alternation expression.
    fn parse_alternation(&mut self) -> Result<Expr, ParsingError> {
        let mut expr = self.parse_concatenation()?;
        while self.check(Token::Pipe) {
            let rhs = self.parse_concatenation()?;
            expr = Expr::Alternation {
                lhs: Box::new(expr),
                rhs: Box::new(rhs),
            };
        }
        Ok(expr)
    }

    /// Parses a concatenation expression.
    fn parse_concatenation(&mut self) -> Result<Expr, ParsingError> {
        let mut expr = self.parse_repetition()?;
        while self.check(Token::Concat) {
            let rhs = self.parse_repetition()?;
            expr = Expr::Concatenation {
                lhs: Box::new(expr),
                rhs: Box::new(rhs),
            }
        }
        Ok(expr)
    }

    /// Parses a repetition expression.
    fn parse_repetition(&mut self) -> Result<Expr, ParsingError> {
        let expr = self.parse_grouping()?;
        if self.check(Token::Star) {
            return Ok(Expr::Repetition(Box::new(expr), Times::ZeroOrMore));
        }
        if self.check(Token::Plus) {
            return Ok(Expr::Repetition(Box::new(expr), Times::OneOrMore));
        }
        if self.check(Token::Question) {
            return Ok(Expr::Repetition(Box::new(expr), Times::ZeroOrOne));
        }
        Ok(expr)
    }

    /// Parses a grouping expression.
    fn parse_grouping(&mut self) -> Result<Expr, ParsingError> {
        if self.check(Token::LeftParen) {
            let expr = self.parse_alternation()?;
            return match self.iter.next() {
                Some(Token::RightParen) => Ok(Expr::Grouping(Box::new(expr))),
                _ => Err(ParsingError::UnmatchedParenthesis),
            };
        }
        self.parse_matching()
    }

    /// Parses a matching expression.
    fn parse_matching(&mut self) -> Result<Expr, ParsingError> {
        match self.iter.next() {
            Some(Token::Char(ch)) => Ok(Expr::Matching(ch)),
            Some(tok) => Err(ParsingError::UnexpectedToken(tok)),
            None => Err(ParsingError::Eof),
        }
    }

    /// Matches the next token against the passed token.  In the case
    /// of a match, it returns true and advances the iterator cursor.
    fn check(&mut self, tok: Token) -> bool {
        self.iter.next_if_eq(&tok).is_some()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn scan_empty() {
        assert_eq!(scan(""), &[]);
    }

    #[test]
    fn scan_not_quantifiable() {
        assert_eq!(scan("a++"), &[Token::Char('a'), Token::Plus, Token::Plus]);
    }

    #[test]
    fn scan_complex() {
        assert_eq!(
            scan("a+(bc?|d+e+)*f"),
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
        assert_eq!(parse(&scan("a")), Ok(Expr::Matching('a')));
    }

    #[test]
    fn parse_concat() {
        assert_eq!(
            parse(&scan("ab")),
            Ok(Expr::Concatenation {
                lhs: Box::new(Expr::Matching('a')),
                rhs: Box::new(Expr::Matching('b')),
            })
        );
    }

    #[test]
    fn parse_repetition() {
        assert_eq!(
            parse(&scan("a*b")),
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
            parse(&scan("a|b|c")),
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
            parse(&scan("a|((b|c)|d)")),
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
            parse(&scan("a+(b?|c)*d")),
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
            parse(&scan("((a|b)|c")),
            Err(ParsingError::UnmatchedParenthesis)
        )
    }

    #[test]
    fn parse_error_not_quantifiable() {
        assert_eq!(
            parse(&scan("a++")),
            Err(ParsingError::UnexpectedToken(Token::Plus))
        )
    }

    #[test]
    fn parse_error_unexpected_token() {
        assert_eq!(
            parse(&scan("|a")),
            Err(ParsingError::UnexpectedToken(Token::Pipe))
        )
    }

    #[test]
    fn parse_error_eof() {
        assert_eq!(parse(&scan("a|")), Err(ParsingError::Eof))
    }

    #[test]
    fn ast_to_string() {
        assert_eq!(
            parse(&scan("a+(b?|c)*d")).unwrap().to_string(),
            "a+(b?|c)*d"
        )
    }
}
