use std::env;
use std::process::{Command, Stdio};

fn link_lib(name: &str) {
    println!("cargo:rustc-link-lib={}", name);
}

fn link_search(path: &str) {
    println!("cargo:rustc-link-search={}", path);
}

fn output(c: &mut Command) -> String {
    let output = match c.stderr(Stdio::inherit()).output() {
        Ok(s) => s,
        Err(e) => panic!("failed to execute {:?}: {}", c, e),
    };
    if !output.status.success() {
        panic!("command exited with error: {:?}: {}", c, output.status);
    }
    String::from_utf8(output.stdout).unwrap()
}

fn llvm_config() -> String {
    if let Ok(path) = env::var("LLVM_CONFIG") {
        return path;
    }

    for candidate in [
        "llvm-config",
        "llvm-config-20",
        "llvm-config-19",
        "llvm-config-18",
        "llvm-config-17",
        "llvm-config-16",
        "llvm-config-15",
        "llvm-config-14",
        "llvm-config-13",
        "llvm-config-12",
        "llvm-config-11",
    ] {
        let status = Command::new(candidate)
            .arg("--version")
            .stdout(Stdio::null())
            .stderr(Stdio::null())
            .status();
        if matches!(status, Ok(status) if status.success()) {
            return candidate.into();
        }
    }

    panic!("failed to find llvm-config; set LLVM_CONFIG or install llvm-config on PATH");
}

fn main() {
    let mut cmd = Command::new(llvm_config());
    cmd.arg("--libs")
        .arg("core")
        .arg("x86codegen")
        .arg("--system-libs");

    let llvm_config_output = output(&mut cmd);
    println!("llvm-config output: {}", llvm_config_output);

    for arg in llvm_config_output.split_whitespace() {
        if arg.starts_with("-l") {
            link_lib(&arg[2..]);
            continue
        }

        if arg.starts_with("-L") {
            link_search(&arg[2..]);
        }
    }

    link_lib("c");
    link_lib("z");
}
