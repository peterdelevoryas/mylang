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
- Uniform function call syntax (https://en.wikipedia.org/wiki/Uniform_Function_Call_Syntax)
- Compile-time constants (replacing macros or static data)

Some more features that I want to add, but haven't finished implementing yet:

- Tuple destructuring
- Generics
- Fat pointer primitive (`ptr` + `len`): `[]T`, like Rust `&[T]` slice type
- Sum types (like Rust `enum`'s)
- Pattern matching (like Rust `match`)
- Module system
- SIMD primitive types

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

fn main(argc: i32, argv: **i8) -> i32 {
    let file: *i8 = null;
    for let i = 1; i < argc; i += 1 {
        if strcmp(argv[i], "-h") == 0 {
            printf("usage: cu [options] file...\n");
            continue;
        }
        file = argv[i];
        break;
    }
    if file == null {
        printf("missing file argument\n");
        return 1;
    }
    printf("file = %s\n", file);

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
fn dot(a: *vec3, b: *vec3) {
    a.x *= b.x;
    a.y *= b.y;
    a.z *= b.z;
}

fn main(argc: i32, argv: **i8) -> i32 {
    let a: vec3 = { x: 1.0, y: 2.0, z: 3.0, };
    let b: vec3 = { x: 2.0, y: 3.0, z: 4.0, };
    dot(&a, &b);
    print(a);

    return 0;
}
```
