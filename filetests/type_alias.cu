type FILE struct {}
type int = i32;
type long = i64;
type char = i8;

fn printf(fmt: *char, ...) -> i32;
fn fread(ptr: *char, size: long, nitems: long, stream: *FILE) -> long;
fn fopen(path: *char, mode: *char) -> *FILE;
fn fclose(stream: *FILE) -> int;
fn ftell(stream: *FILE) -> long;
fn fseek(stream: *FILE, offset: long, whence: int) -> int;

fn main(argc: int, argv: **char) -> int {
    if argc < 2 {
        printf("not enough arguments\n");
    }
    return 0;
}

