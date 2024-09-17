//! Regexp compiler library for x86.
//!
//! The only supported metacharacters are `* + ? | ( )`.  Their
//! precedence, from weakest to strongest binding, is first
//! alternation (`|`), then concatenation, and finally the repetition
//! operators (`*`, `+` and `?`).

use std::{
    cell::RefCell,
    collections::HashSet,
    fmt::{self, Display, Formatter},
    iter::Peekable,
    rc::{Rc, Weak},
    result, slice,
};

/// Regular expression result.
pub type Result<T> = result::Result<T, Error>;

/// Regular expression error.
#[derive(Debug, PartialEq)]
pub enum Error {
    UnmatchedParenthesis,
    UnexpectedToken(Token),
    Eof,
}

impl Display for Error {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Error::UnmatchedParenthesis => write!(f, "unmatched parenthesis"),
            Error::UnexpectedToken(tok) => write!(f, "unexpected token: {tok}"),
            Error::Eof => write!(f, "EOF"),
        }
    }
}

/// Lexical token.
#[derive(Debug, Copy, Clone, PartialEq)]
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

impl Display for Token {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Token::Concat => write!(f, "CONCAT"),
            Token::Star => write!(f, "STAR"),
            Token::Plus => write!(f, "PLUS"),
            Token::Question => write!(f, "QUESTION"),
            Token::Pipe => write!(f, "PIPE"),
            Token::LeftParen => write!(f, "LEFT_PAREN"),
            Token::RightParen => write!(f, "RIGHT_PAREN"),
            Token::Char(ch) => write!(f, "{ch}"),
        }
    }
}

/// Performs the lexical analysis of the provided regular expression.
/// It returns the token list.
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
#[derive(Debug, PartialEq)]
pub enum Expr {
    Alternation { lhs: Box<Expr>, rhs: Box<Expr> },
    Concatenation { lhs: Box<Expr>, rhs: Box<Expr> },
    Repetition(Box<Expr>, Times),
    Grouping(Box<Expr>),
    Matching(char),
}

impl Display for Expr {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match self {
            Expr::Alternation { lhs, rhs } => write!(f, "{lhs}|{rhs}"),
            Expr::Concatenation { lhs, rhs } => write!(f, "{lhs}{rhs}"),
            Expr::Repetition(expr, op) => write!(f, "{expr}{op}"),
            Expr::Grouping(expr) => write!(f, "({expr})"),
            Expr::Matching(ch) => write!(f, "{ch}"),
        }
    }
}

/// Number of times the inner expression of an [`Expr::Repetition`]
/// expression is repeated.
#[derive(Debug, PartialEq)]
pub enum Times {
    ZeroOrMore,
    OneOrMore,
    ZeroOrOne,
}

impl Display for Times {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match self {
            Times::ZeroOrMore => write!(f, "*"),
            Times::OneOrMore => write!(f, "+"),
            Times::ZeroOrOne => write!(f, "?"),
        }
    }
}

/// Performs the syntactic analysis of the provided tokens.  It
/// returns the parsed AST.
pub fn parse(tokens: &[Token]) -> Result<Expr> {
    Parser::parse(tokens)
}

/// Regular expression parser.
struct Parser<'a> {
    /// Token peekable iterator.
    tokens: Peekable<slice::Iter<'a, Token>>,
}

impl Parser<'_> {
    /// Performs the syntactic analysis of the provided tokens.  It
    /// returns the parsed AST.
    fn parse(tokens: &[Token]) -> Result<Expr> {
        let mut parser = Parser {
            tokens: tokens.iter().peekable(),
        };
        let expr = parser.parse_alternation()?;
        match parser.tokens.next() {
            Some(tok) => Err(Error::UnexpectedToken(*tok)),
            None => Ok(expr),
        }
    }

    /// Parses an alternation expression.
    fn parse_alternation(&mut self) -> Result<Expr> {
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
    fn parse_concatenation(&mut self) -> Result<Expr> {
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
    fn parse_repetition(&mut self) -> Result<Expr> {
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
    fn parse_grouping(&mut self) -> Result<Expr> {
        if self.check(Token::LeftParen) {
            let expr = self.parse_alternation()?;
            return match self.tokens.next() {
                Some(Token::RightParen) => Ok(Expr::Grouping(Box::new(expr))),
                _ => Err(Error::UnmatchedParenthesis),
            };
        }
        self.parse_matching()
    }

    /// Parses a matching expression.
    fn parse_matching(&mut self) -> Result<Expr> {
        match self.tokens.next() {
            Some(Token::Char(ch)) => Ok(Expr::Matching(*ch)),
            Some(tok) => Err(Error::UnexpectedToken(*tok)),
            None => Err(Error::Eof),
        }
    }

    /// Compares the next token with `tok`.  In equal, returns true
    /// and advances the iterator cursor.
    fn check(&mut self, tok: Token) -> bool {
        self.tokens.next_if_eq(&&tok).is_some()
    }
}

/// A state of the non-deterministic finite automata (NFA).
struct State {
    /// Unique state identifier.
    id: u64,

    /// Character to match.
    ch: Option<char>,

    /// Forward states.
    fwd: Vec<Rc<RefCell<State>>>,

    /// Backward states.
    bck: Vec<Weak<RefCell<State>>>,
}

impl State {
    /// Returns both the forward and the backward links of the state.
    fn links(&self) -> Vec<Rc<RefCell<State>>> {
        let mut states = Vec::new();
        for fwd in &self.fwd {
            states.push(Rc::clone(fwd));
        }
        for bck in &self.bck {
            states.push(Weak::upgrade(bck).expect("upgrade backward link"));
        }
        states
    }
}

/// Regular expression compiler.
#[derive(Default)]
pub struct Compiler {
    /// Next state identifier.
    next_state_id: u64,
}

impl Compiler {
    /// Compile regular expression AST.
    pub fn compile_ast(expr: &Expr) -> Result<String> {
        // TODO: implement function.
        let mut comp = Compiler::default();
        let (_nfa, _) = comp.nfa(expr);
        Ok(format!("TODO: compile {expr}"))
    }

    /// Compile regular expression.
    pub fn compile_regexp(regexp: &str) -> Result<String> {
        let tokens = scan(regexp);
        let expr = parse(&tokens)?;
        Self::compile_ast(&expr)
    }

    /// Returns the NFA corresponding to the provided AST.  The
    /// returned tuple is of the form `(start_state, end_state)`.
    fn nfa(&mut self, expr: &Expr) -> (Rc<RefCell<State>>, Rc<RefCell<State>>) {
        match expr {
            Expr::Alternation { lhs, rhs } => {
                let split = self.new_state();
                let join = self.new_state();
                let (lhs_start, lhs_end) = self.nfa(lhs);
                let (rhs_start, rhs_end) = self.nfa(rhs);
                split.borrow_mut().fwd = vec![lhs_start, rhs_start];
                lhs_end.borrow_mut().fwd = vec![Rc::clone(&join)];
                rhs_end.borrow_mut().fwd = vec![Rc::clone(&join)];
                (split, join)
            }
            Expr::Concatenation { lhs, rhs } => {
                let (lhs_start, lhs_end) = self.nfa(lhs);
                let (rhs_start, rhs_end) = self.nfa(rhs);
                lhs_end.borrow_mut().fwd = vec![rhs_start];
                (lhs_start, rhs_end)
            }
            Expr::Repetition(expr, times) => {
                let (expr_start, expr_end) = self.nfa(expr);
                let join = self.new_state();
                match times {
                    Times::ZeroOrOne => {
                        let split = self.new_state();
                        split.borrow_mut().fwd = vec![expr_start, Rc::clone(&join)];
                        expr_end.borrow_mut().fwd = vec![Rc::clone(&join)];
                        (split, join)
                    }
                    Times::ZeroOrMore => {
                        let split = self.new_state();
                        split.borrow_mut().fwd = vec![expr_start, Rc::clone(&join)];
                        expr_end.borrow_mut().fwd = vec![Rc::clone(&join)];
                        expr_end.borrow_mut().bck = vec![Rc::downgrade(&split)];
                        (split, join)
                    }
                    Times::OneOrMore => {
                        expr_end.borrow_mut().fwd = vec![Rc::clone(&join)];
                        expr_end.borrow_mut().bck = vec![Rc::downgrade(&expr_start)];
                        (expr_start, join)
                    }
                }
            }
            Expr::Grouping(expr) => self.nfa(expr),
            Expr::Matching(ch) => {
                let expr = self.new_state();
                let end = self.new_state();
                expr.borrow_mut().ch = Some(*ch);
                expr.borrow_mut().fwd = vec![Rc::clone(&end)];
                (expr, end)
            }
        }
    }

    /// Returns a new [`State`].  Every generated state has a unique
    /// identifier.
    fn new_state(&mut self) -> Rc<RefCell<State>> {
        let state = Rc::new(RefCell::new(State {
            id: self.next_state_id,
            ch: None,
            fwd: Vec::new(),
            bck: Vec::new(),
        }));
        self.next_state_id += 1;
        state
    }
}

/// Regular expression.
pub struct Regexp {
    /// Internal NFA.
    nfa: Rc<RefCell<State>>,

    /// Current index in the input string.
    idx: usize,

    /// Current states of the internal NFA.
    states: Vec<Rc<RefCell<State>>>,
}

impl Regexp {
    /// Returns a new [`Regexp`] from the provided regular expression
    /// AST.
    pub fn from_ast(expr: &Expr) -> Regexp {
        let mut comp = Compiler::default();
        let (nfa, _) = comp.nfa(expr);
        let mut re = Regexp {
            nfa: Rc::clone(&nfa),
            idx: 0,
            states: Vec::new(),
        };
        re.states = Self::walk(nfa, &mut HashSet::new());
        re
    }

    /// Returns a new [`Regexp`] from the provided regular expression.
    pub fn from_regexp(regexp: &str) -> Result<Regexp> {
        let tokens = scan(regexp);
        let expr = parse(&tokens)?;
        Ok(Regexp::from_ast(&expr))
    }

    /// Resets the internal state of the compiler.
    fn reset(&mut self) {
        self.idx = 0;
        self.states = Self::walk(Rc::clone(&self.nfa), &mut HashSet::new());
    }

    /// Returns whether the regular expression matches `text`.
    pub fn matches(&mut self, text: &str) -> bool {
        self.reset();
        loop {
            if self.idx == text.chars().count() {
                return self
                    .states
                    .iter()
                    .any(|state| state.borrow().links().is_empty());
            }

            let Some(sch) = text.chars().nth(self.idx) else {
                return false;
            };

            let mut visited = HashSet::new();
            let mut states = Vec::new();
            for state in &self.states {
                let Some(ch) = state.borrow().ch else {
                    continue;
                };
                if ch != sch {
                    continue;
                }
                for lnk in state.borrow().links() {
                    states.append(&mut Self::walk(Rc::clone(&lnk), &mut visited));
                }
            }
            self.states = states;
            self.idx += 1;
        }
    }

    /// Walks the NFA looking for character and end nodes.
    fn walk(state: Rc<RefCell<State>>, visited: &mut HashSet<u64>) -> Vec<Rc<RefCell<State>>> {
        if visited.contains(&state.borrow().id) {
            return Vec::new();
        }
        visited.insert(state.borrow().id);

        if state.borrow().ch.is_some() || state.borrow().links().is_empty() {
            return vec![Rc::clone(&state)];
        }

        state
            .borrow()
            .links()
            .iter()
            .flat_map(|lnk| Self::walk(Rc::clone(lnk), visited))
            .collect()
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
        assert_eq!(parse(&scan("((a|b)|c")), Err(Error::UnmatchedParenthesis));
    }

    #[test]
    fn parse_error_not_quantifiable() {
        assert_eq!(
            parse(&scan("a++")),
            Err(Error::UnexpectedToken(Token::Plus))
        );
    }

    #[test]
    fn parse_error_unexpected_token() {
        assert_eq!(parse(&scan("|a")), Err(Error::UnexpectedToken(Token::Pipe)));
    }

    #[test]
    fn parse_error_eof() {
        assert_eq!(parse(&scan("a|")), Err(Error::Eof));
    }

    #[test]
    fn expr_to_string() {
        assert_eq!(
            parse(&scan("a+(b?|c)*d")).unwrap().to_string(),
            "a+(b?|c)*d"
        );
    }

    #[test]
    fn regexp_matches() {
        for (regexp, text, want) in &[
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
            ("a+(b?|c)*d", "a", false),
            ("(a?)*b", "ab", true),
            ("(a?)*b", "b", true),
            ("(a?)*b", "", false),
            ("(a*)*b", "", false),
            ("(a?)+b", "", false),
            ("(a*)+b", "", false),
            (&("a?".repeat(50) + &"a".repeat(50)), &"a".repeat(50), true),
            ("ƒoo", "ƒoo", true),
        ] {
            let mut re = Regexp::from_regexp(regexp).unwrap();
            assert_eq!(re.matches(text), *want, "matching {regexp} against {text}");
        }
    }
}
