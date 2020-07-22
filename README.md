# Overview

This language is approximately equal to C, but with some essential modern features:

- Modern syntax
- Type inference
- Local variable shadowing
- Stronger typing (explicit casting required)
- Auto-deref: `ptr.field` implicitly becomes `(*ptr).field`
- `bool` type
- Tuples
- First-class `()` type and value replaces `void`
- Compile-time constants (replacing macros or static data)
- Sum types (like Rust `enum`'s)
- Pattern matching (`if let ...`)

Some more features that I want to add, but haven't finished implementing yet:

- Generics
- Fat pointer primitive (`ptr` + `len`): `[]T`, like Rust `&[T]` slice type
- Module system
- SIMD primitive types
- Uniform function call syntax
- `match` expression (like Rust)

The `filetests/` directory contains examples of all language features, below
some are reproduced with added comments.

```
// Hello world

// You can (and must) declare externally defined functions to link with.
fn printf(fmt: *i8, ...) -> i32;

fn main(argc: i32, argv: **i8) -> i32 {
    printf("hello world\n");
    return 0;
}
```

```
// Local variable declaration

fn printf(fmt: *i8, ...) -> i32;
fn main(argc: i32, argv: **i8) -> i32 {
    // Basic local variable declaration.
    let x: i32 = 1;

    // Type annotation is optional, and shadowing is allowed.
    let x = 2;
    printf("%d\n", x); // 2

    return 0;
}
```

```
// Parse compiler args

fn printf(fmt: *i8, ...) -> i32;
fn strcmp(x: *i8, y: *i8) -> i32;

// Returns (filename, -)
fn parse_args(argc: i32, argv: **i8) -> (*i8, bool) {
    let file: *i8 = null;
    let help = false;
    for let i = 1; i < argc; i += 1 {
        let arg = argv[i];
        if strcmp(arg, "-h") == 0 {
            help = true;
            continue;
        }
        file = arg;
    }
    return (file, help);
}

fn main(argc: i32, argv: **i8) -> i32 {
    let (file, help) = parse_args(argc, argv);
    printf("help = %d, file = %s\n", help, file);
    if help {
        printf("usage: mylangc [-h] <file>\n");
        return 1;
    }
    if file == null {
        printf("missing file argument\n");
        return 1;
    }
    return 0;
}
```

```
// Structs and auto-deref

fn printf(fmt: *i8, ...) -> i32;

type vec3 struct {
    x: f32,
    y: f32,
    z: f32,
}

fn print(v: vec3) {
    let x = v.x as f64;
    let y = v.y as f64;
    let z = v.z as f64;
    printf("%f %f %f\n", x, y, z);
}

// Field accesses on pointers to structs are auto-deref'd.
fn dot(x: *vec3, y: *vec3) -> f32 {
    return x.x * y.x + x.y * y.y + x.z * y.z;
}

fn main(argc: i32, argv: **i8) -> i32 {
    let a: vec3 = { x: 1.0, y: 2.0, z: 3.0, };
    let b: vec3 = { x: 2.0, y: 3.0, z: 4.0, };
    print(a);
    print(b);

    let ab = dot(&a, &b);
    printf("dot a, b = %f\n", ab as f64);

    return 0;
}
```

```
// Pattern matching with `if let`

fn printf(fmt: *i8, ...);

type expr enum {
    int(i32),
    string(*i8),
}

fn print(e: expr) {
    if let int(i) = e {
        printf("%d\n", i);
    }
    if let string(s) = e {
        printf("%s\n", s);
    }
}

fn main() -> i32 {
    let e = expr.int(1);
    print(e);

    e = expr.string("hello world");
    print(e);

    return 0;
}
```
