use std::env;
use std::fmt;
use std::fs;
use std::path::Path;
use std::path::PathBuf;
use std::process::Command;

use filecheck::Checker;
use filecheck::CheckerBuilder;

#[test]
fn filetests() {
    let manifest_dir = env!("CARGO_MANIFEST_DIR");
    let manifest_dir = PathBuf::from(manifest_dir);
    let filetests_dir = manifest_dir.join("filetests");
    let compiler_path = &manifest_dir.join("target/debug/mylangc");

    let test_paths = find_files(filetests_dir);
    let mut failed = vec![];
    for path in &test_paths {
        if let Err(e) = run_test(compiler_path, &path) {
            println!("test {} failed: {}", path.display(), e);
            failed.push(path);
        }
    }
    let passed = test_paths.len() - failed.len();
    if failed.len() != 0 {
        panic!("failed tests: {:?}", failed);
    }
    println!("{} passed, {} failed", passed, failed.len());
}

/// All file paths in all subdirectories are returned.
fn find_files(dir: PathBuf) -> Vec<PathBuf> {
    let mut files = vec![];
    let mut dirs = vec![dir];
    while let Some(dir) = dirs.pop() {
        for entry in fs::read_dir(dir).unwrap() {
            let entry = entry.unwrap();
            let path = entry.path();
            if path.is_dir() {
                dirs.push(path);
            } else {
                files.push(path);
            }
        }
    }
    files
}

trait Context: Sized {
    type Output;
    fn ctx(self, s: &str) -> Self::Output;
}

impl<T, E: fmt::Display> Context for Result<T, E> {
    type Output = Result<T, String>;
    fn ctx(self, s: &str) -> Self::Output {
        match self {
            Ok(x) => Ok(x),
            Err(e) => {
                let e = format!("{}: {}", s, e);
                Err(e)
            }
        }
    }
}

fn run_test(compiler: &Path, file: &Path) -> Result<(), String> {
    let mut compiler = Command::new(compiler);
    compiler.arg(file);
    let output = run_command(&mut compiler)?;
    println!("{:?} {}", compiler, output);

    if !Path::new("./a.out").exists() {
        return Err("a.out doesn't exist".into());
    }
    let mut a_out = Command::new("./a.out");
    let output = run_command(&mut a_out)?;
    fs::remove_file("./a.out").ctx("removing a.out")?;
    println!("{:?}\n{}", a_out, output);

    let checker = file_checker(file);
    let success = checker.check(&output, &()).ctx("filecheck")?;
    if !success {
        let (_, e) = checker.explain(&output, &()).ctx("filecheck explain")?;
        let e = format!("filecheck:\n{}", e);
        return Err(e);
    }
    Ok(())
}

fn run_command(command: &mut Command) -> Result<String, String> {
    let output = match command.output() {
        Err(e) => {
            let e = format!("{:?}: {}", command, e);
            return Err(e);
        }
        Ok(x) => x,
    };
    let mut bytes = vec![];
    bytes.extend_from_slice(&output.stdout);
    bytes.extend_from_slice(&output.stderr);
    let s = String::from_utf8(bytes).ctx("utf8 error")?;
    if !output.status.success() {
        return Err(s);
    }
    Ok(s)
}

/// Comments in the file are parsed as checker directives.
fn file_checker(file: &Path) -> Checker {
    let file = fs::read_to_string(file).unwrap();
    let mut checker = CheckerBuilder::new();
    for line in file.lines() {
        let line = line.trim_start();
        if !line.starts_with("// ") {
            continue;
        }
        let line = &line[3..];
        println!("checker directive {:?}", line);
        if let Err(e) = checker.directive(line) {
            panic!("directive error: {}", e);
        }
    }
    checker.finish()
}
