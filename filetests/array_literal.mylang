fn printf(fmt: *i8, ...);

fn main() -> i32 {
    // check: 1 2 3
    let x = [1, 2, 3];
    printf("%d %d %d\n", x[0], x[1], x[2]);

    // nextln: 2 3 4
    x[0] = x[1];
    x[1] = x[2];
    x[2] = 4;
    printf("%d %d %d\n", x[0], x[1], x[2]);

    let x: [0]i32 = [];

    return 0;
}
