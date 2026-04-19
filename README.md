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
// Explicit libc namespace import.

import libc;

function main() -> i32 {
    let p = libc.malloc(64);
    libc.printf("ptr = %p\n", p);
    return 0;
}
```

```
// Hello world

import libc;

function main(argc: i32, argv: **i8) -> i32 {
    libc.printf("hello world\n");
    return 0;
}
```

```
// Local variable declaration

import libc;

function main(argc: i32, argv: **i8) -> i32 {
    // Basic local variable declaration.
    let x: i32 = 1;

    // Type annotation is optional, and shadowing is allowed.
    let x = 2;
    libc.printf("%d\n", x); // 2

    return 0;
}
```

```
// Parse compiler args

import libc;

// Returns (filename, -)
function parse_args(argc: i32, argv: **i8) -> (*i8, bool) {
    let file: *i8 = null;
    let help = false;
    for let i = 1; i < argc; i += 1 {
        let arg = argv[i];
        if libc.strcmp(arg, "-h") == 0 {
            help = true;
            continue;
        }
        file = arg;
    }
    return (file, help);
}

function main(argc: i32, argv: **i8) -> i32 {
    let (file, help) = parse_args(argc, argv);
    libc.printf("help = %d, file = %s\n", help, file);
    if help {
        libc.printf("usage: ono [-h] <file>\n");
        return 1;
    }
    if file == null {
        libc.printf("missing file argument\n");
        return 1;
    }
    return 0;
}
```

```
// 4x4 Matrix multiplication

import libc;

type Mat4 = [4][4]f32;

function matmul(a: Mat4, b: Mat4) -> Mat4 {
    let xx = a[0][0] * b[0][0] + a[0][1] * b[1][0] + a[0][2] * b[2][0] + a[0][3] * b[3][0];
    let xy = a[0][0] * b[0][1] + a[0][1] * b[1][1] + a[0][2] * b[2][1] + a[0][3] * b[3][1];
    let xz = a[0][0] * b[0][2] + a[0][1] * b[1][2] + a[0][2] * b[2][2] + a[0][3] * b[3][2];
    let xw = a[0][0] * b[0][3] + a[0][1] * b[1][3] + a[0][2] * b[2][3] + a[0][3] * b[3][3];

    let yx = a[1][0] * b[0][0] + a[1][1] * b[1][0] + a[1][2] * b[2][0] + a[1][3] * b[3][0];
    let yy = a[1][0] * b[0][1] + a[1][1] * b[1][1] + a[1][2] * b[2][1] + a[1][3] * b[3][1];
    let yz = a[1][0] * b[0][2] + a[1][1] * b[1][2] + a[1][2] * b[2][2] + a[1][3] * b[3][2];
    let yw = a[1][0] * b[0][3] + a[1][1] * b[1][3] + a[1][2] * b[2][3] + a[1][3] * b[3][3];

    let zx = a[2][0] * b[0][0] + a[2][1] * b[1][0] + a[2][2] * b[2][0] + a[2][3] * b[3][0];
    let zy = a[2][0] * b[0][1] + a[2][1] * b[1][1] + a[2][2] * b[2][1] + a[2][3] * b[3][1];
    let zz = a[2][0] * b[0][2] + a[2][1] * b[1][2] + a[2][2] * b[2][2] + a[2][3] * b[3][2];
    let zw = a[2][0] * b[0][3] + a[2][1] * b[1][3] + a[2][2] * b[2][3] + a[2][3] * b[3][3];

    let wx = a[3][0] * b[0][0] + a[3][1] * b[1][0] + a[3][2] * b[2][0] + a[3][3] * b[3][0];
    let wy = a[3][0] * b[0][1] + a[3][1] * b[1][1] + a[3][2] * b[2][1] + a[3][3] * b[3][1];
    let wz = a[3][0] * b[0][2] + a[3][1] * b[1][2] + a[3][2] * b[2][2] + a[3][3] * b[3][2];
    let ww = a[3][0] * b[0][3] + a[3][1] * b[1][3] + a[3][2] * b[2][3] + a[3][3] * b[3][3];

    return [[xx, xy, xz, xw],
            [yx, yy, yz, yw],
            [zx, zy, zz, zw],
            [wx, wy, wz, ww]];
}

function print(a: Mat4) {
    for let i = 0; i < 4; i += 1 {
        for let j = 0; j < 4; j += 1 {
            libc.printf("%10.4f  ", a[i][j] as f64);
        }
        libc.printf("\n");
    }
}

function main() -> i32 {
    let a = [[ 1.0,  9.0,  3.0,  4.0],
             [ 2.0, -4.0,  5.0,  0.5],
             [-3.0,  1.0,  6.0, -3.0],
             [ 6.0, 10.0, 11.0,  1.0]];
    let b = matmul(a, a);
    print(a);
    print(b);
    return 0;
}
```

```
// Structs and auto-deref

import libc;

type Vec3 = struct {
    x: f32,
    y: f32,
    z: f32,
};

function print(v: Vec3) {
    let x = v.x as f64;
    let y = v.y as f64;
    let z = v.z as f64;
    libc.printf("%f %f %f\n", x, y, z);
}

// Field accesses on pointers to structs are auto-deref'd.
function dot(x: *Vec3, y: *Vec3) -> f32 {
    return x.x * y.x + x.y * y.y + x.z * y.z;
}

function main(argc: i32, argv: **i8) -> i32 {
    let a: Vec3 = { x: 1.0, y: 2.0, z: 3.0, };
    let b: Vec3 = { x: 2.0, y: 3.0, z: 4.0, };
    print(a);
    print(b);

    let ab = dot(&a, &b);
    libc.printf("dot a, b = %f\n", ab as f64);

    return 0;
}
```

```
// Pattern matching with `if let`

import libc;

type Expr = enum {
    Int(i32),
    String(*i8),
};

function print(e: Expr) {
    if let Int(i) = e {
        libc.printf("%d\n", i);
    }
    if let String(s) = e {
        libc.printf("%s\n", s);
    }
}

function main() -> i32 {
    let e = Expr.Int(1);
    print(e);

    e = Expr.String("hello world");
    print(e);

    return 0;
}
```
