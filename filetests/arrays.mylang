fn printf(fmt: *i8, ...);

fn main() -> i32 {
    let x: [256]i8;
    for let i = 0; i < 256; i += 1 {
        x[i] = i as i8;
    }
    // check: 0
    // nextln: 1
    // nextln: 2
    // nextln: 3
    // check: 128
    // nextln: 129
    // check: 255
    for let i = 0; i < 256; i += 1 {
        printf("%d\n", x[i]);
    }
    printf("\n");

    return 0;
}
