#![allow(unused_variables, unused_imports, dead_code)]

use std::env;
use std::fs;
use std::process::exit;
use std::fmt;
use std::ops::Deref;

mod syntax;
mod ir0;
mod codegen;

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
    let mut struct_types = vec![];
    let mut func_decls = vec![];
    let mut func_bodys = vec![];
    while p.token != syntax::EOF {
        if p.token == syntax::TYPE {
            let struct_type = p.parse_struct_type();
            struct_types.push(struct_type);
            continue;
        }

        let decl = p.parse_func_decl();
        let id = func_decls.len();
        func_decls.push(decl);
        if p.token == syntax::SEMICOLON {
            p.next();
            continue;
        }

        let body = p.parse_block();
        let body = syntax::FuncBody {
            id: id,
            body: body,
        };
        func_bodys.push(body);
    }
    println!("{:?}", struct_types);
    println!("{:?}", func_decls);
    println!("{:?}", func_bodys);

    let (func_decls, func_bodys, types) = ir0::build(&func_decls, &func_bodys);
    println!("{:?}", func_decls);
    println!("{:?}", func_bodys);
    println!("{:?}", types);

    unsafe {
        codegen::emit_object(&func_decls, &func_bodys, &types);
    }

    ld("a.o", "a.out");
    let _ = fs::remove_file("a.o");
}

fn ld(path: &str, out: &str) {
    use std::process::Command;
    let mut gcc = Command::new("gcc");
    gcc.arg(path);
    gcc.arg("-o");
    gcc.arg(out);

    let output = match gcc.output() {
        Err(e) => {
            println!("error linking object file: {}", e);
            error();
        }
        Ok(x) => x,
    };

    let stdout = std::string::String::from_utf8_lossy(&output.stdout);
    let stderr = std::string::String::from_utf8_lossy(&output.stderr);
    print!("{}{}", stdout, stderr);
}
