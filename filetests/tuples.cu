fn printf(fmt: *i8, ...) -> i32;

fn print(x: (i32, *i8)) {
    printf("%d %s\n", x.0, x.1);
}

fn map(x: (i32, *i8)) -> (i32, *i8) {
    return (x.0 + 1, &x.1[1]);
}

fn main() -> i32 {
    // check: 1 2
    let x = (1, "2");
    print(x);

    // check 2 3
    let x = (1, "23");
    let x = map(x);
    print(x);

    return 0;
}
