fn printf(fmt: *i8, ...);

type data struct {
    x: i8,
    y: i16, 
    z: i32,
}

fn main() -> i32 {
    let x = 1;
    let p = &x;
    // check: 1
    printf("%d\n", *p);

    let x: data = {
        x: 0,
        y: 1,
        z: 2,
    };
    let p = &x;
    let y = *p;
    // check: 0 1 2
    printf("%d %d %d\n", y.x, y.y, y.z);
    return 0;
}
