// Lexer
use std::str::CharIndices;
use std::iter::Peekable;
use std::fmt;

#[derive(Debug, Clone, PartialEq)]
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

#[derive(Debug, PartialEq, Clone)]
pub enum Token<'a> {
    Ident(&'a str),
    Literal(&'a str),
    Comment(&'a str),
    Semicolon,
    
    Add, Sub, Mul, Div, Pow,
    Equ,

    Oparen, Cparen
}
#[allow(dead_code)]
mod tl {
    use super::Token::{self, *};
    pub const IDENT  : Token<'static> = Ident("");
    pub const LITERAL: Token<'static> = Literal("");
    pub const COMMENT: Token<'static> = Comment("");
    pub const SEMICOLON: Token<'static> = Semicolon;
    pub const ADD: Token<'static> = Add;
    pub const SUB: Token<'static> = Sub;
    pub const MUL: Token<'static> = Mul;
    pub const DIV: Token<'static> = Div;
    pub const POW: Token<'static> = Pow;
    pub const EQU: Token<'static> = Equ;
    pub const OPAREN: Token<'static> = Oparen;
    pub const CPAREN: Token<'static> = Cparen;
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
            Oparen => f.write_str("'('")?,
            Cparen => f.write_str("')'")?,
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
            '(' => Ok(Token::Oparen),
            ')' => Ok(Token::Cparen),
            x => Err(format!("Unrecognised Token {x:?}"))
        }))
    }
}

// Parser
#[derive(Debug, PartialEq)]
enum UchTopLevel<'a> {
    Define(&'a str, UchExpr<'a>),
    Goal(UchExpr<'a>)
}
#[derive(Debug, PartialEq)]
enum Oper { Add, Sub, Mul, Div, Pow }
#[derive(Debug, PartialEq)]
enum UchExpr<'a> {
    Ident(&'a str),
    Number(f64),
    UnOp(Oper, Box<UchExpr<'a>>),
    BiOp(Oper, Box<UchExpr<'a>>, Box<UchExpr<'a>>),
}

struct Parser<'a> {
    inner: Peekable<Lexer<'a>>
}
impl<'a> Parser<'a> {
    fn new(inner: Lexer<'a>) -> Self {
        Parser { inner: inner.peekable() }
    }
    fn pull(&mut self) -> Option<<Lexer<'a> as Iterator>::Item> {
        let mut r = self.inner.next()?;
        while r.1.is_ok() && r.1.as_ref().unwrap().is_comment() {
            r = self.inner.next()?;
        }
        // println!("\t\x1b[35mPULLING\x1b[0m {r:?}");
        Some(r)
    }
    fn peek_pull(&mut self) -> Option<<Lexer<'a> as Iterator>::Item> {
        let mut r = self.inner.peek()?;
        while r.1.is_ok() && r.1.as_ref().unwrap().is_comment() {
            if let (loc, Err(msg)) = self.inner.next()? {
                return Some((loc, Err(msg)))
            }
            r = self.inner.peek()?;
        }
        // println!("\t\x1b[35mPEEK PULLING\x1b[0m {r:?}");
        Some((r.0.clone(), r.1.clone()))
    }
    fn consume<'b: 'a>(&mut self, test: Token<'b>) -> Result<Token, String> {
        //match {let x = self.pull(); println!("\t\x1b[35mCONSUMING\x1b[0m {test} GOT {x:?}"); x } {
        match self.pull() {
            None => Err(format!("Expected {test}, found nothing")),
            Some((_, Err(msg))) => Err(msg),
            Some((_, Ok(toc))) => if toc.var_eq(&test) { Ok(toc) } 
                                  else {Err(format!("Expected {test}, got {toc}"))}
        }
    }
    fn test<'b: 'a>(&mut self, test: Token<'b>) -> Result<bool, String> {
        //match {let x = self.peek_pull(); println!("\t\x1b[35mTESTING\x1b[0m {test} VS {x:?}"); x } {
        match self.peek_pull() {
            None => Ok(false),
            Some((_, Err(msg))) => Err(msg),
            Some((_, Ok(toc))) => Ok(toc.var_eq(&test)) 
        }
    }
}
#[allow(dead_code)]
impl<'a> Parser<'a> {
    // Parsing functions
    // these come in pairs `parse_x` and `test_x` self explanitory
    // `test_x` can only (AND MUST ONLY) lookahead one token.
    pub fn parse_atom(&mut self) -> Result<UchExpr<'a>, String> {
        // <atom> ::= <ident> | <literal>
        let x = self.inner.next();
        if x.is_none() { 
            return Err(format!("Expected Either {} or {}, got nothing", tl::IDENT, tl::LITERAL));
        }
        let (_, x) = x.unwrap();
        if x.is_err() { return Err(x.unwrap_err()); }
        let x = x.unwrap(); 
        match x {
            Token::Ident(id) => Ok(UchExpr::Ident(id)),
            Token::Literal(lit) => Ok(UchExpr::Number(lit.parse().expect("INTERNAL ERROR [float parse]"))),
            x => Err(format!("Expected Either {} or {}, got {x}", tl::IDENT, tl::LITERAL)),
        }
    }
    pub fn test_atom(&mut self) -> Result<bool, String> {
        Ok(self.test(tl::IDENT)? || self.test(tl::LITERAL)?)
    }

    pub fn parse_tight(&mut self) -> Result<UchExpr<'a>, String> {
        // <tight> ::= <atom> | <atom> "^" <tight> | "(" <expr> ")" 
        //           | "-" <tight> | "+" <tight>
        if self.test(tl::SUB)? {
            self.pull();
            return Ok(UchExpr::UnOp(Oper::Sub, Box::new(self.parse_tight()?)));
        }
        if self.test(tl::ADD)? {
            self.pull();
            return self.parse_tight();
        }
        if self.test(tl::OPAREN)? {
            self.pull();
            let cur = self.parse_expr()?;
            self.consume(tl::CPAREN)?;
            return Ok(cur)
        }
        let cur = self.parse_atom()?;
        if self.test(tl::POW)? {
            self.pull();
            let exp = self.parse_tight()?;
            return Ok(UchExpr::BiOp(Oper::Pow, Box::new(cur), Box::new(exp)));
        }
        return Ok(cur);
    }
    pub fn test_tight(&mut self) -> Result<bool, String> {
        Ok(self.test_atom()? || self.test(tl::OPAREN)?
        || self.test(tl::SUB)? || self.test(tl::ADD)?)
    }
    // lol
    pub fn parse_factor(&mut self) -> Result<UchExpr<'a>, String> {
        // <factor> ::= <tight> | <tight> <factor>
        let mut cur = self.parse_tight()?;
        while self.test_tight()? {
            let nxt = self.parse_tight()?;
            cur = UchExpr::BiOp(Oper::Mul, Box::new(cur), Box::new(nxt));
        }
        Ok(cur)
    }
    pub fn test_factor(&mut self) -> Result<bool, String> {
        self.test_tight()
    }
    pub fn parse_term(&mut self) -> Result<UchExpr<'a>, String> {
        // <term> ::= <factor> | <factor> "*" <term> | <factor> "/" <term>
        let cur = self.parse_factor()?;
        return Ok(if self.test(tl::MUL)? {
            self.pull();
            let nxt = self.parse_term()?;
            UchExpr::BiOp(Oper::Mul, Box::new(cur), Box::new(nxt))
        } else if self.test(tl::DIV)? {
            self.pull();
            let nxt = self.parse_term()?;
            UchExpr::BiOp(Oper::Div, Box::new(cur), Box::new(nxt))
        } else {
            cur
        })
    }
    pub fn test_term(&mut self) -> Result<bool, String> {
        self.test_factor()
    }
    pub fn parse_phrase(&mut self) -> Result<UchExpr<'a>, String> {
        // <phrase> ::= <term> | <term> "+" <phrase> | <term> "-" <phrase>
        let cur = self.parse_term()?;
        return Ok(if self.test(tl::ADD)? {
            self.pull();
            let next = self.parse_phrase()?;
            UchExpr::BiOp(Oper::Add, Box::new(cur), Box::new(next))
        } else if self.test(tl::SUB)? {
            self.pull();
            let next = self.parse_phrase()?;
            UchExpr::BiOp(Oper::Sub, Box::new(cur), Box::new(next))
        } else {
            cur
        })
    }
    pub fn test_phrase(&mut self) -> Result<bool, String> {
        self.test_term()
    }

    pub fn parse_expr(&mut self) -> Result<UchExpr<'a>, String> { self.parse_phrase() }
}

impl<'a> Iterator for Parser<'a> {
    type Item = (Loc, Result<UchTopLevel<'a>, String>);
    fn next(&mut self) -> Option<Self::Item> {
        let (sloc, res) = self.pull()?;
        if res.is_err() { return Some((sloc, Err(res.unwrap_err()))); }
        let res = res.unwrap();
        return match res {
            Token::Ident(name) => Some((sloc, (|| {
                self.consume(tl::EQU)?;
                let cont = self.parse_expr()?;
                self.consume(tl::SEMICOLON)?;
                Ok(UchTopLevel::Define(name, cont))
            })())),
            Token::Equ         => Some((sloc, (|| {
                let cont = self.parse_expr()?;
                self.consume(tl::SEMICOLON)?;
                Ok(UchTopLevel::Goal(cont))
            })())),
            x => Some((sloc, Err(format!("Expected {} or {} got {x}",tl::IDENT, tl::EQU))))
        }
    }
}

// Checker
use std::cell::UnsafeCell;
use std::collections::HashMap;
type Either<A, B> = Result<A, B>; // because connotation.
struct Detacher<'input, 'output> {
    // note: `pool` MUSTN'T BE RESIZED, IT IS ASSUMED TO BE A FIXED SIZE BUFFER
    // ALTHOUGH IT IS A `String`, INITIALISE IT WITH THE CORRECT AMOUNT OF
    // CAPACITY OR FACE DEFINITE SEGFAULTS.
    // DEBUG MODE WILL IMPLEMENT MINIMAL CHECKS, AND IT WILL FORCE ABORT TO
    // AVOID SEGFAULT.
    lookup: HashMap<&'input str, &'output str>,
    pool: UnsafeCell<String>
} impl<'a, 'b> Detacher<'a, 'b> {
    unsafe fn inner_ref<'h, 'i: 'h>(&'h mut self) -> &'i String {
        &* self.pool.get()
    }
    unsafe fn inner_mut<'h, 'i: 'h>(&'h mut self) -> &'i mut String {
        use std::ptr::addr_of_mut;
        let mut temp = self.pool.get();
        &mut** addr_of_mut!(temp).cast::<*mut String>()
    }
    fn detach(&mut self, inp: &'a str) -> &'b str {
        if !self.lookup.contains_key(inp) { unsafe {
            #[cfg(debug_assertions)]
            let init = self.inner_ref().capacity();
            let start = self.inner_ref().len();
            self.inner_mut().push_str(inp);
            let end = self.inner_ref().len();
            #[cfg(debug_assertions)]
            if self.inner_ref().capacity() != init { panic!("Detachment pool resized") }
            let new = &self.inner_ref()[start .. end];
            self.lookup.insert(inp, new);
        } }
        return self.lookup.get(inp).unwrap()
    }
}
type RegInd  = u16;
type InstInd = u16;
enum UchInstr<'a> {
    LoadConst(Value, RegInd),
    LoadExt(&'a str, RegInd),
    BiOp(Oper, RegInd, RegInd, RegInd), 
    UnOp(Oper, RegInd, RegInd),
    Call(&'a str, RegInd, Vec<RegInd>),
    Return(RegInd),
}
// IDEA:
//     When type-checking we "interpret" the program, but only on a type level
//     We keep track of registers, but they're indices into an entity table.
//     The entity table. The entity table contain indices to a type table.
//     The type table which allows us to handle type deduction and generic
//     typing.
// TT/Tt refers to TypeTable.
#[derive(Debug)]
struct TypeTable(
    Vec<Either<TtType, Vec<TtRestrict>>>
); impl TypeTable {
    fn unify(&mut self, oth: &TypeTable, ind: TtInd) -> TtInd {
        
    }
    fn insert(&mut self, oth: &TypeTable, ind: TtInd) {

    }
}
type TtInd = u16;
#[derive(PartialEq, Debug, Hash)]
enum TtTypes {
    Logical,
    Number,
    Func(TtInd, Vec<TtInd>)
}
enum Restrictions {
    Add(TtInd),
    Sub
}


// Runtime
enum Value {
//    Logical(bool),
    Number(f64)
}
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
    let parser = Parser::new(lexer);
    for (loc, res) in parser {
        match res {
            Ok(tok) => println!("{tok:?}"),
            Err(msg) => {
                println!("ERROR:test.sima:{}:{}: {msg}", loc.line, loc.col);
                let pos = buff.as_str().lines().skip(loc.line.saturating_sub(2)).next();
                match pos {
                    Some(line) => println!("    {line}"),
                    None => ()
                }
            }
        }
    }
    Ok(())
}


#[cfg(test)]
mod tests {
    use super::*;
    // I've already written the lexer, but I don't want to break this.
    #[test]
    fn lexer_comments() {
        let source = "// Comment 1\n// Comment 2";
        let mut lex = Lexer::new(source);
        assert_eq!(lex.next(),
            Some((Loc{ line: 1, col: 1}, Ok(Token::Comment("// Comment 1"))))
        );
        assert_eq!(lex.next(),
            Some((Loc{ line: 2, col: 1}, Ok(Token::Comment("// Comment 2"))))
        );
    }
    #[test]
    fn lexer_number() {
        let source = "1 2.2 03 04.5";
        let mut lex = Lexer::new(source);
        assert_eq!(lex.next(), Some((Loc{line:1,col:1}, Ok(Token::Literal("1")))));
        assert_eq!(lex.next(), Some((Loc{line:1,col:3}, Ok(Token::Literal("2.2")))));
        assert_eq!(lex.next(), Some((Loc{line:1,col:7}, Ok(Token::Literal("03")))));
        assert_eq!(lex.next(), Some((Loc{line:1,col:10}, Ok(Token::Literal("04.5")))));
    }
    #[test]
    fn lexer_ident() {
        let source = "ascii ὐνικοδε";
        let mut lex = Lexer::new(source);
        assert_eq!(lex.next(), Some((Loc{line:1,col:1}, Ok(Token::Ident("ascii")))));
        assert_eq!(lex.next(), Some((Loc{line:1,col:7}, Ok(Token::Ident("ὐνικοδε")))));
    }
    fn lexer_symbols() {
        let source = "+-*/()=;";
        let mut lex = Lexer::new(source);
        assert_eq!(lex.next(), Some((Loc{line:1,col:1}, Ok(Token::Add))));
        assert_eq!(lex.next(), Some((Loc{line:1,col:2}, Ok(Token::Sub))));
        assert_eq!(lex.next(), Some((Loc{line:1,col:3}, Ok(Token::Mul))));
        assert_eq!(lex.next(), Some((Loc{line:1,col:4}, Ok(Token::Div))));
        assert_eq!(lex.next(), Some((Loc{line:1,col:5}, Ok(Token::Oparen))));
        assert_eq!(lex.next(), Some((Loc{line:1,col:6}, Ok(Token::Cparen))));
        assert_eq!(lex.next(), Some((Loc{line:1,col:7}, Ok(Token::Equ))));
        assert_eq!(lex.next(), Some((Loc{line:1,col:8}, Ok(Token::Semicolon))));
    }
    #[test]
    fn parser_atom() {
        let source = "ident 1337";
        let mut par = Parser::new(Lexer::new(source));
        assert_eq!(par.parse_atom(), Ok(UchExpr::Ident("ident")));
        assert_eq!(par.parse_atom(), Ok(UchExpr::Number(1337f64)));
    }
    // further testing needed
}
