fn printf(fmt: *i8, ...);
fn strlen(s: *i8) -> i64;
fn strcmp(s1: *i8, s2: *i8) -> i32;

type string struct {
    ptr: *i8,
}

fn make_string(ptr: *i8) -> string {
    return { ptr: ptr };
}

fn cmp(x: string, y: string) -> i32 {
    return strcmp(x.ptr, y.ptr);
}

fn len(s: string) -> i64 {
    return strlen(s.ptr);
}

fn main() -> i32 {

    // check: abcdefg
    // nextln: 7
    let s: string = make_string("abcdefg");
    printf("%s\n", s.ptr);
    printf("%d\n", len(s));

    // nextln: 0
    printf("%d\n", cmp(s, s));

    return 0;
}
