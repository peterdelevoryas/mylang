fn printf(fmt: *i8, ...);

fn main() -> i32 {
    // check: 1 2 3
    let ((x, y), z) = ((1, 2), 3);
    printf("%d %d %d\n", x, y, z);
    return 0;
}

