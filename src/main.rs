// Lexer
use std::str::CharIndices;
use std::iter::Peekable;

#[derive(Debug, Clone)]
struct Loc{
    line: usize,
    col: usize,
}
impl Loc {
    fn new() -> Self { Loc{ line: 1, col: 1 } }
    fn desc(&mut self) { self.line += 1; self.col = 1; }
    fn forw(&mut self) { self.col += 1; }
}

struct Lexer<'a> {
    cur: Loc,
    base: &'a str,
    inner: Peekable<CharIndices<'a>>,
}
impl<'a> Lexer<'a> {
    fn new(base: &'a str) -> Self {
        Lexer { cur: Loc::new(), base, inner: base.char_indices().peekable() }
    }
    fn pull(&mut self) -> Option<(usize, char)> {
        let (e, c) = self.inner.next()?;
        if c == '\n' { self.cur.desc() }
        else { self.cur.forw() }
        Some((e, c))
    }
    fn pull_char(&mut self) -> Option<char> {
        self.pull().map(|x| x.1)
    }
    fn skip_whitespace(&mut self) {
        while let Some(&(_, x)) = self.inner.peek() {
            if !x.is_whitespace() { break }
            self.pull();
        }
    }
    fn test(&mut self, t: char) -> bool {
        match self.inner.peek() {
            Some(&(_, x)) => x == t,
            None => false,
        }
    }
}
enum Token<'a> {
    Ident(&'a str),
    Literal(&'a str),
    Comment(&'a str),
    Semicolon,
}

// Parser
// Checker
// Main
use std::io::{self, Read};
use std::fs::File;
pub fn main() -> io::Result<()> {
    let buff = {
        let mut file = File::open("test.sima")?;
        let mut buff = String::with_capacity(file.metadata()?.len() as usize);
        file.read_to_string(&mut buff)?;
        buff
    };

    let lexer = Lexer::new(buff.as_str());
    Ok(())
}
