fn printf(fmt: *i8, ...);

fn main() -> i32 {

    // check: true
    if true {
        printf("true\n");
    }

    // not: false
    if false {
        printf("false\n");
    }

    return 0;
}
