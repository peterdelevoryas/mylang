#![allow(unused_variables, unused_imports, dead_code)]

use std::env;
use std::fs;
use std::process::exit;
use std::fmt;
use std::ops::Deref;

mod syntax;

fn usage() {
    println!("\
OVERVIEW: r2 LLVM compiler

USAGE: r2 [options] file...

OPTIONS:
    -h | --help             Display available options.
");
}

fn parse_args() -> std::string::String {
    let mut path = None;
    for arg in env::args().skip(1) {
        if arg == "-h" || arg == "--help" {
            usage();
            error();
        }
        if let Some(path) = path {
            println!("multiple file arguments: {:?}, {:?}", path, arg);
            usage();
            error();
        }
        path = Some(arg);
    }
    match path {
        None => {
            println!("missing file argument");
            usage();
            error();
        }
        Some(path) => path,
    }
}

fn error() -> ! {
    println!("# compilation error, exiting with error code 1...");
    exit(1)
}

#[derive(Copy, Clone, PartialEq, Eq, Hash, Default)]
pub struct String(u16);

impl Deref for String {
    type Target = str;
    fn deref(&self) -> &str {
        let i = self.0 as usize;
        unsafe { &INTERN[i] }
    }
}

impl fmt::Debug for String {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{:?}", self.deref())
    }
}

impl fmt::Display for String {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.deref())
    }
}

static mut INTERN: Vec<std::string::String> = Vec::new();

pub fn intern(s: &str) -> String {
    let intern = unsafe { &mut INTERN };
    for (i, interned) in intern.iter().enumerate() {
        let i = i as u16;
        if s == interned {
            return String(i);
        }
    }
    let i = intern.len() as u16;
    intern.push(s.into());
    String(i)
}

fn print_cursor(text: &str, start: usize, end: usize) {
    println!();
    let line_start = match text[..start].rfind('\n') {
        Some(i) => i + 1,
        None => 0,
    };
    let line_end = start + text[start..].find('\n').unwrap_or(text.len() - start);
    println!("{}", &text[line_start..line_end]);

    for b in text[line_start..start].bytes() {
        let c = if b.is_ascii_whitespace() { b as char } else { ' ' };
        print!("{}", c);
    }
    for _ in start..end {
        print!("^");
    }
}

fn main() {
    let path = &parse_args();
    let text = &match fs::read_to_string(path) {
        Err(e) => {
            println!("unable to read {:?}: {}", path, e);
            error();
        }
        Ok(s) => s,
    };
    println!("{:?}", text);
    let mut p = syntax::Parser {
        text: text,
        start: 0,
        end: 0,
        token: syntax::EOF,
    };
    p.next();
    let mut funcs = Vec::new();
    while p.token != syntax::EOF {
        let func = p.parse_func();
        funcs.push(func);
    }

    println!("{:?}", funcs);
}
