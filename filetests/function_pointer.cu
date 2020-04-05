fn printf(fmt: *i8, ...) -> i32;

fn add(x: i32, y: i32) -> i32 {
    let z = x + y;
    printf("%d + %d = %d\n", x, y, z);
    return z;
}

fn main() -> i32 {
    // check: 1 + 2 = 3
    let f: *fn(i32, i32) -> i32 = &add;
    f(1, 2);

    return 0;
}
