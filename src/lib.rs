//! Regexp compiler library for x86.
//!
//! The only supported metacharacters are `* + ? | ( )`.  Their
//! precedence, from weakest to strongest binding, is first
//! alternation (`|`), then concatenation, and finally the repetition
//! operators (`*`, `+` and `?`).

use std::{cell::RefCell, fmt, iter, rc::Rc, slice, str};

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

/// Parsing error.
#[derive(Debug, Eq, PartialEq)]
pub enum ParsingError {
    UnmatchedParenthesis,
    UnexpectedToken(Token),
    Eof,
}

/// The Abstract Syntax Tree (AST) of a regular expression.
///
/// Implements the following grammar (from weakest to strongest
/// binding):
///
/// ```text
/// alternation   = concatenation {PIPE concatenation} .
/// concatenation = repetition {CONCAT repetition} .
/// repetition    = grouping [STAR | PLUS | QUESTION] .
/// grouping      = LEFT_PAREN alternation RIGHT_PARENT | matching .
/// matching      = CHAR .
/// ```
///
/// The alternation and concatenation operators are left-associative.
///
/// Note that the `CONCAT` token is not present in the string
/// representation of the regular expression.  It is added by the
/// lexical scanner to represent two adjacent expressions.
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum Expr {
    Alternation { lhs: Box<Expr>, rhs: Box<Expr> },
    Concatenation { lhs: Box<Expr>, rhs: Box<Expr> },
    Repetition(Box<Expr>, Times),
    Grouping(Box<Expr>),
    Matching(char),
}

/// Number of times the inner expression of an [`Expr::Repetition`]
/// expression is repeated.
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum Times {
    ZeroOrMore,
    OneOrMore,
    ZeroOrOne,
}

impl fmt::Display for Times {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Times::ZeroOrMore => write!(f, "*"),
            Times::OneOrMore => write!(f, "+"),
            Times::ZeroOrOne => write!(f, "?"),
        }
    }
}

impl fmt::Display for Expr {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Expr::Alternation { lhs, rhs } => write!(f, "{lhs}|{rhs}"),
            Expr::Concatenation { lhs, rhs } => write!(f, "{lhs}{rhs}"),
            Expr::Repetition(expr, op) => write!(f, "{expr}{op}"),
            Expr::Grouping(expr) => write!(f, "({expr})"),
            Expr::Matching(ch) => write!(f, "{ch}"),
        }
    }
}

/// Performs the syntactic analysis of the provided tokens.  It
/// returns the parsed AST.
pub fn parse(tokens: &[Token]) -> Result<Expr, ParsingError> {
    Parser::parse(tokens)
}

/// Stores the state of the regular expression parser.
struct Parser<'a> {
    /// Token peekable iterator.
    tokens: iter::Peekable<slice::Iter<'a, Token>>,
}

impl Parser<'_> {
    /// Performs the syntactic analysis of the provided tokens.  It
    /// returns the parsed AST.
    fn parse(tokens: &[Token]) -> Result<Expr, ParsingError> {
        let mut parser = Parser {
            tokens: tokens.iter().peekable(),
        };
        let expr = parser.parse_alternation()?;
        match parser.tokens.next() {
            Some(tok) => Err(ParsingError::UnexpectedToken(*tok)),
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
            return match self.tokens.next() {
                Some(Token::RightParen) => Ok(Expr::Grouping(Box::new(expr))),
                _ => Err(ParsingError::UnmatchedParenthesis),
            };
        }
        self.parse_matching()
    }

    /// Parses a matching expression.
    fn parse_matching(&mut self) -> Result<Expr, ParsingError> {
        match self.tokens.next() {
            Some(Token::Char(ch)) => Ok(Expr::Matching(*ch)),
            Some(tok) => Err(ParsingError::UnexpectedToken(*tok)),
            None => Err(ParsingError::Eof),
        }
    }

    /// Matches the next token against the passed token.  In the case
    /// of a match, it returns true and advances the iterator cursor.
    fn check(&mut self, tok: Token) -> bool {
        self.tokens.next_if_eq(&&tok).is_some()
    }
}

/// Stores the state of the regular expression emulator.
pub struct Emulator {
    /// String to match.
    s: String,

    /// Current states of the emulator.
    states: Vec<(Rc<RefCell<State>>, usize)>,
}

/// A state of the internal non-deterministic finite automata (NFA).
#[derive(Default)]
struct State {
    /// Character to match.
    ch: Option<char>,

    /// Links to next states.
    fwd: Vec<Rc<RefCell<State>>>,
}

impl Emulator {
    /// Returns a new [`Emulator`].
    pub fn new(expr: &Expr, s: &str) -> Emulator {
        let (start, _) = Emulator::compile(expr);
        Emulator {
            s: s.into(),
            states: vec![(start, 0)],
        }
    }

    /// Creates the internal NFA.  It returns the start and end states
    /// of the provided expression.
    fn compile(expr: &Expr) -> (Rc<RefCell<State>>, Rc<RefCell<State>>) {
        match expr {
            Expr::Alternation { lhs, rhs } => {
                let split = Rc::new(RefCell::new(State::default()));
                let join = Rc::new(RefCell::new(State::default()));
                let (lhs_start, lhs_end) = Self::compile(lhs);
                let (rhs_start, rhs_end) = Self::compile(rhs);
                split.borrow_mut().fwd = vec![lhs_start, rhs_start];
                lhs_end.borrow_mut().fwd = vec![Rc::clone(&join)];
                rhs_end.borrow_mut().fwd = vec![Rc::clone(&join)];
                (split, join)
            }
            Expr::Concatenation { lhs, rhs } => {
                let (lhs_start, lhs_end) = Self::compile(lhs);
                let (rhs_start, rhs_end) = Self::compile(rhs);
                lhs_end.borrow_mut().fwd = vec![rhs_start];
                (lhs_start, rhs_end)
            }
            Expr::Repetition(expr, times) => {
                let (expr_start, expr_end) = Self::compile(expr);
                let join = Rc::new(RefCell::new(State::default()));
                match times {
                    Times::ZeroOrOne => {
                        let split = Rc::new(RefCell::new(State::default()));
                        split.borrow_mut().fwd = vec![expr_start, Rc::clone(&join)];
                        expr_end.borrow_mut().fwd = vec![Rc::clone(&join)];
                        (split, join)
                    }
                    Times::ZeroOrMore => {
                        let split = Rc::new(RefCell::new(State::default()));
                        split.borrow_mut().fwd = vec![expr_start, Rc::clone(&join)];
                        expr_end.borrow_mut().fwd = vec![Rc::clone(&join), Rc::clone(&split)];
                        (split, join)
                    }
                    Times::OneOrMore => {
                        expr_end.borrow_mut().fwd = vec![Rc::clone(&join), Rc::clone(&expr_start)];
                        (expr_start, join)
                    }
                }
            }
            Expr::Grouping(expr) => Self::compile(expr),
            Expr::Matching(ch) => {
                let expr = Rc::new(RefCell::new(State {
                    ch: Some(*ch),
                    ..State::default()
                }));
                (Rc::clone(&expr), expr)
            }
        }
    }

    /// Emulates one step forward.
    pub fn step(&mut self) -> Option<bool> {
        if self.states.is_empty() {
            return Some(false);
        }

        let mut new_states = Vec::new();
        for (state, mut idx) in &self.states {
            let state = state.borrow();
            if let Some(ch) = state.ch {
                let Some(sch) = self.s.chars().nth(idx) else {
                    continue;
                };
                if ch != sch {
                    continue;
                }
                idx += 1;
            }
            if state.fwd.is_empty() {
                if idx == self.s.len() {
                    return Some(true);
                }
                continue;
            }
            for fwd in &state.fwd {
                new_states.push((Rc::clone(fwd), idx))
            }
        }
        self.states = new_states;

        None
    }

    /// Emulates the regular expression.
    pub fn emulate(&mut self) -> bool {
        loop {
            if let Some(res) = self.step() {
                return res;
            }
        }
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
        );
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
        );
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
        );
    }

    #[test]
    fn parse_error_unmatched_parenthesis() {
        assert_eq!(
            parse(&scan("((a|b)|c")),
            Err(ParsingError::UnmatchedParenthesis)
        );
    }

    #[test]
    fn parse_error_not_quantifiable() {
        assert_eq!(
            parse(&scan("a++")),
            Err(ParsingError::UnexpectedToken(Token::Plus))
        );
    }

    #[test]
    fn parse_error_unexpected_token() {
        assert_eq!(
            parse(&scan("|a")),
            Err(ParsingError::UnexpectedToken(Token::Pipe))
        );
    }

    #[test]
    fn parse_error_eof() {
        assert_eq!(parse(&scan("a|")), Err(ParsingError::Eof));
    }

    #[test]
    fn expr_to_string() {
        assert_eq!(
            parse(&scan("a+(b?|c)*d")).unwrap().to_string(),
            "a+(b?|c)*d"
        );
    }

    #[test]
    fn emulator_emulate() {
        for (re, s, res) in &[
            ("a", "a", true),
            ("a", "b", false),
            ("abc", "abc", true),
            ("abc", "abd", false),
            ("a|b", "a", true),
            ("a|b", "b", true),
            ("a|b", "c", false),
            ("a|b|c", "a", true),
            ("a|b|c", "b", true),
            ("a|b|c", "c", true),
            ("a|b|c", "d", false),
            ("(a|b)c", "ac", true),
            ("(a|b)c", "bc", true),
            ("(a|b)c", "cc", false),
            ("(a|b)c", "aa", false),
            ("a?", "", true),
            ("a?", "a", true),
            ("a?", "b", false),
            ("a?b", "ab", true),
            ("a?b", "b", true),
            ("a?b", "a", false),
            ("a*", "a", true),
            ("a*", "aa", true),
            ("a*", "", true),
            ("a*", "b", false),
            ("a*a", "a", true),
            ("a*a", "aaaaa", true),
            ("a*b", "aaabb", false),
            ("a*b", "aaaab", true),
            ("a*b", "b", true),
            ("a+", "a", true),
            ("a+", "aa", true),
            ("a+", "", false),
            ("a+b", "ab", true),
            ("a+b", "aab", true),
            ("a+a", "a", false),
            ("a+b", "b", false),
            ("a+(b?|c)*d", "aabbd", true),
            ("a+(b?|c)*d", "ad", true),
            ("a+(b?|c)*d", "accd", true),
            ("a+(b?|c)*d", "cd", false),
            ("a+(b?|c)*d", "bd", false),
            // ("a+(b?|c)*d", "a", false), // Infinite loop.
            ("(a?)*b", "ab", true),
            ("(a?)*b", "b", true),
            // ("(a?)*b", "", false), // Infinite loop.
            // ("(a*)*b", "", false), // Infinite loop.
            // ("(a?)+b", "", false), // Infinite loop.
            // ("(a*)+b", "", false), // Infinite loop.
        ] {
            assert_eq!(Emulator::new(&parse(&scan(re)).unwrap(), s).emulate(), *res);
        }
    }
}
