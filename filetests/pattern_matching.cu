fn printf(fmt: *i8, ...);

type expr enum {
    int(i32),
    string(*i8),
}

fn print(e: expr) {
    if let int(i) = e {
        printf("%d\n", i);
    }
    if let string(s) = e {
        printf("%s\n", s);
    }
}

fn main() -> i32 {
    // check: 1
    let e = expr.int(1);
    print(e);
    // nextln: hello world
    e = expr.string("hello world");
    print(e);

    return 0;
}
