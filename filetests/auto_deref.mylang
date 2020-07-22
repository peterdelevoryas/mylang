fn printf(fmt: *i8, ...);

type person struct {
    name: *i8,
    height: f32,
    weight: f32,
    age: i32,
}

// field accesses on pointers to structs are auto-deref'd
fn print_person(p: *person) {
    printf("name: %s\n", p.name);
    printf("height: %f\n", p.height as f64);
    printf("weight: %f\n", p.weight as f64);
    printf("age: %d\n", p.age);
}

fn main() -> i32 {
    let bob: person = {
        name: "bob",
        height: 5.128,
        weight: 175.89,
        age: 32,
    };
    // check: bob
    // nextln: 5.128000
    // nextln: 175.889999
    // nextln: 32
    print_person(&bob);

    return 0;
}
