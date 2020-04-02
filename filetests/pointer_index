fn printf(fmt: *i8, ...);
fn malloc(size: i64) -> *i8;
fn free(ptr: *i8);

fn main() -> i32 {
    // check: 0123456789
    let x = malloc(10);
    for let i = 0; i < 10; i += 1 {
        x[i] = i as i8;
    }
    for let i = 0; i < 10; i += 1 {
        printf("%d", x[i]);
    }
    printf("\n");
    free(x);
    return 0;
}
