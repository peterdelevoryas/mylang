fn printf(fmt: *i8, ...);

fn print(b: bool) {
    printf("%d\n", b);
}

fn main() -> i32 {
    // check: 1
    print(1 == 1);
    // nextln: 0
    print(1 == 2);

    // nextln: 0
    print(1 != 1);
    // nextln: 1
    print(1 != 2);

    // nextln: 1
    print(1 < 2);
    // nextln: 0
    print(1 < 0);

    // nextln: 0
    print(1 > 2);
    // nextln: 1
    print(2 > 1);

    // nextln: 1
    print(1 <= 1);
    // nextln: 1
    print(1 <= 2);
    // nextln: 0
    print(2 <= 1);

    // nextln: 1
    print(1 >= 1);
    // nextln: 1
    print(2 >= 1);
    // nextln: 0
    print(1 >= 2);

    // check: 1
    print(1.0 == 1.0);
    // nextln: 0
    print(1.0 == 2.0);

    // nextln: 0
    print(1.0 != 1.0);
    // nextln: 1
    print(1.0 != 2.0);

    // nextln: 1
    print(1.0 < 2.0);
    // nextln: 0
    print(1.0 < 0.0);

    // nextln: 0
    print(1.0 > 2.0);
    // nextln: 1
    print(2.0 > 1.0);

    // nextln: 1
    print(1.0 <= 1.0);
    // nextln: 1
    print(1.0 <= 2.0);
    // nextln: 0
    print(2.0 <= 1.0);

    // nextln: 1
    print(1.0 >= 1.0);
    // nextln: 1
    print(2.0 >= 1.0);
    // nextln: 0
    print(1.0 >= 2.0);


    return 0;
}
