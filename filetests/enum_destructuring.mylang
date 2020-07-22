fn printf(fmt: *i8, ...);

type expr enum {
    int(i32),
    string(*i8),
}

fn main() -> i32 {
    // check: 1
    let e = expr.int(1);
    let int(i) = e;
    printf("%d\n", i);

    // nextln: hello world
    let x = expr.string("hello world");
    let string(s) = x;
    printf("%s\n", s);

    return 0;
}

