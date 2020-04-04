fn printf(fmt: *i8, ...);

fn main() -> i32 {
    // check: 0123456789
    for let i = 0; i < 10; i += 1 {
        printf("%d", i);
    }
    printf("\n");
    return 0;
}
