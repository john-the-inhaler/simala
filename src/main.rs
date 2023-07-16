// Lexer
use std::str::CharIndices;
use std::iter::Peekable;
use std::fmt;

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

#[derive(Debug, PartialEq)]
enum Token<'a> {
    Ident(&'a str),
    Literal(&'a str),
    Comment(&'a str),
    Semicolon,
    
    Add, Sub, Mul, Div, Pow,
    Equ
}
impl<'a> Token<'a> {
    fn is_comment(&self) -> bool { 
        match self { Token::Comment(_) => true, _ => false }
    }
    fn var_eq(&self, oth: &Self) -> bool {
        use std::mem::discriminant;
        discriminant(self) == discriminant(oth)
    }
}
impl<'a> fmt::Display for Token<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> Result<(), fmt::Error> {
        use Token::*;
        match self {
            Add => f.write_str("'+'")?,
            Sub => f.write_str("'+'")?,
            Mul => f.write_str("'+'")?,
            Div => f.write_str("'+'")?,
            Pow => f.write_str("'+'")?,
            Equ => f.write_str("'='")?,
            Semicolon => f.write_str("';'")?,
            Ident(_) => f.write_str("Identifier")?,
            Literal(_) => f.write_str("Literal")?,
            Comment(_) => f.write_str("Comment")?, 
        }
        Ok(())
    }
}
impl<'a> Iterator for Lexer<'a> {
    type Item = (Loc, Result<Token<'a>, String>);
    fn next(&mut self) -> Option<Self::Item> {
        self.skip_whitespace();
        let sloc = self.cur.clone();
        let (start, init) = self.pull()?;
        Some((sloc, match init {
            x if x.is_alphabetic() => {
                while let Some(&(_, chr)) = self.inner.peek() {
                    if !(chr.is_alphanumeric() || chr == '_') {
                        break
                    }
                    self.pull();
                }
                let end = self.inner.peek().map(|x| x.0).unwrap_or(self.base.len());
                Ok(Token::Ident(&self.base[start .. end]))
            }
            x if x.is_ascii_digit() => {
                while let Some(&(_, chr)) = self.inner.peek() {
                    if !chr.is_ascii_digit() {
                        break
                    }
                    self.pull();
                }
                if self.test('.') {
                    self.pull();
                    while let Some(&(_, chr)) = self.inner.peek() {
                        if !chr.is_ascii_digit() {
                            break
                        }
                        self.pull();
                    }
                }
                let end = self.inner.peek().map(|x| x.0).unwrap_or(self.base.len());
                Ok(Token::Literal(&self.base[start .. end]))
            }
            '/' => 'divvy: {
                if !self.test('/') {
                    break 'divvy Ok(Token::Div);
                }
                while !self.test('\n') && self.inner.peek().is_some() {
                    self.pull();
                }
                let end = self.inner.peek().map(|x| x.0).unwrap_or(self.base.len());
                Ok(Token::Comment(&self.base[start .. end]))
            },
            '+' => Ok(Token::Add),
            '*' => Ok(Token::Mul),
            '-' => Ok(Token::Sub),
            '=' => Ok(Token::Equ),
            '^' => Ok(Token::Pow),
            ';' => Ok(Token::Semicolon),
            x => Err(format!("Unrecognised Token {x:?}"))
        }))
    }
}

// Parser
enum UchTopLevel<'a> {
    Define(&'a str, UchExpr<'a>),
    Goal(UchExpr<'a>)
}
enum Oper { Add, Sub, Mul, Div, Pow }
enum UchExpr<'a> {
    Ident(&'a str),
    Number(f64),
    UnOp(Oper, Box<UchExpr<'a>>),
    BiOp(Oper, Box<UchExpr<'a>>, Box<UchExpr<'a>>),
}

struct Parser<'a> {
    inner: Lexer<'a>
}
impl<'a> Parser<'a> {
    fn new(inner: Lexer<'a>) -> Self {
        Parser { inner }
    }
    fn pull(&mut self) -> Option<<Lexer<'a> as Iterator>::Item> {
        let mut r = self.inner.next()?;
        while r.1.is_ok() && r.1.as_ref().unwrap().is_comment() {
            r = self.inner.next()?;
        }
        Some(r)
    }
    fn consume<'b: 'a>(&mut self, test: Token<'b>) -> Result<(), String> {
        match self.inner.next() {
            None => Err(format!("Expected {test}, found nothing")),
            Some((_, Err(msg))) => Err(msg),
            Some((_, Ok(toc))) => if toc.var_eq(&test) { Ok(()) } 
                                  else {Err(format!("Expected {test}, got different {toc}"))}
        }
    }
}



// Checker
// Main
use std::io::{self, Read};
use std::fs::File;
pub fn main() -> io::Result<()> {
    // Open -> Lex -> Parse -> Check(detach) -> Run
    let buff = {
        let mut file = File::open("test.sima")?;
        let mut buff = String::with_capacity(file.metadata()?.len() as usize);
        file.read_to_string(&mut buff)?;
        buff
    };

    let lexer = Lexer::new(buff.as_str());
    for (loc, res) in lexer {
        match res {
            Ok(tok) => println!("{tok:?}"),
            Err(msg) => println!("ERROR:test.sima:{}:{}: {msg}", loc.line, loc.col)
        }
    }
    Ok(())
}
