fn printf(fmt: *i8, ...);
fn malloc(size: i64) -> *i8;
fn free(ptr: *i8);

type data struct {
    x: i32,
    y: i32,
    z: i32,
}

fn print(x: *data) {
    printf("%d %d %d\n", x.x, x.y, x.z);
}

fn main() -> i32 {
    // check: 0 0 0
    // nextln: 1 2 3
    // nextln: 2 4 6
    // nextln: 3 6 9
    // nextln: 4 8 12
    // nextln: 5 10 15
    // nextln: 6 12 18
    // nextln: 7 14 21
    // nextln: 8 16 24
    // nextln: 9 18 27
    let n = 10;
    let x: *data = malloc(sizeof(data) * (n as i64)) as *data;
    for let i = 0; i < n; i += 1 {
        x[i] = { x: i * 1, y: i * 2, z: i * 3, };
    }
    for let i = 0; i < n; i += 1 {
        print(&x[i]);
    }

    return 0;
}
