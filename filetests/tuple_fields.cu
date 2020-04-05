fn printf(fmt: *i8, ...) -> i32;

fn main() -> i32 {
    // check: x = (1, 2)
    let x = (1, "2");
    printf("x = (%d, %s)\n", x.0, x.1);
    return 0;
}
