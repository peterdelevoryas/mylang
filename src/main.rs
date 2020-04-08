use llvm_sys;
use std::env;
use std::fmt;
use std::fs;
use std::ops::Deref;
use std::process::exit;

mod ir;
mod llvm;
mod syntax;

fn usage() {
    println!(
        "\
OVERVIEW: cu LLVM compiler

USAGE: cu [options] file...

OPTIONS:
    -h | --help             Display available options.
    --print-llvm            Display generated LLVM IR.
"
    );
}

struct Args {
    path: std::string::String,
    print_ir: bool,
    print_llvm: bool,
}

fn parse_args() -> Args {
    let mut args = Args {
        path: std::string::String::new(),
        print_ir: false,
        print_llvm: false,
    };
    for arg in env::args().skip(1) {
        if arg == "-h" || arg == "--help" {
            usage();
            error();
        }
        if arg == "--print-llvm" {
            args.print_llvm = true;
            continue;
        }
        if arg == "--print-ir" {
            args.print_ir = true;
            continue;
        }
        if args.path != "" {
            println!("multiple file arguments: {:?}, {:?}", args.path, arg);
            usage();
            error();
        }
        args.path = arg;
    }
    if args.path == "" {
        println!("missing file argument");
        usage();
        error();
    }
    args
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
        let c = if b.is_ascii_whitespace() {
            b as char
        } else {
            ' '
        };
        print!("{}", c);
    }
    for _ in start..end {
        print!("^");
    }
}

fn main() {
    let args = parse_args();
    let text = &match fs::read_to_string(&args.path) {
        Err(e) => {
            println!("unable to read {:?}: {}", args.path, e);
            error();
        }
        Ok(s) => s,
    };
    let module = syntax::parse(text);
    let module = ir::build(&module);
    unsafe {
        let (machine, module) = llvm::build(&module);
        if args.print_llvm {
            llvm_sys::LLVMDumpModule(module);
        }
        llvm::verify(module);
        llvm::emit_object(machine, module);
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
