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
function printf(fmt: *i8, ...) -> i32;

function main(argc: i32, argv: **i8) -> i32 {
    printf("hello world\n");
    return 0;
}
```

```
// Local variable declaration

function printf(fmt: *i8, ...) -> i32;
function main(argc: i32, argv: **i8) -> i32 {
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

function printf(fmt: *i8, ...) -> i32;
function strcmp(x: *i8, y: *i8) -> i32;

// Returns (filename, -)
function ParseArgs(argc: i32, argv: **i8) -> (*i8, bool) {
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

function main(argc: i32, argv: **i8) -> i32 {
    let (file, help) = ParseArgs(argc, argv);
    printf("help = %d, file = %s\n", help, file);
    if help {
        printf("usage: ono [-h] <file>\n");
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
// 4x4 Matrix multiplication

function printf(fmt: *i8, ...) -> i32;

type Mat4 = [4][4]f32;

function MatMul(A: Mat4, B: Mat4) -> Mat4 {
    let xx = A[0][0] * B[0][0] + A[0][1] * B[1][0] + A[0][2] * B[2][0] + A[0][3] * B[3][0];
    let xy = A[0][0] * B[0][1] + A[0][1] * B[1][1] + A[0][2] * B[2][1] + A[0][3] * B[3][1];
    let xz = A[0][0] * B[0][2] + A[0][1] * B[1][2] + A[0][2] * B[2][2] + A[0][3] * B[3][2];
    let xw = A[0][0] * B[0][3] + A[0][1] * B[1][3] + A[0][2] * B[2][3] + A[0][3] * B[3][3];

    let yx = A[1][0] * B[0][0] + A[1][1] * B[1][0] + A[1][2] * B[2][0] + A[1][3] * B[3][0];
    let yy = A[1][0] * B[0][1] + A[1][1] * B[1][1] + A[1][2] * B[2][1] + A[1][3] * B[3][1];
    let yz = A[1][0] * B[0][2] + A[1][1] * B[1][2] + A[1][2] * B[2][2] + A[1][3] * B[3][2];
    let yw = A[1][0] * B[0][3] + A[1][1] * B[1][3] + A[1][2] * B[2][3] + A[1][3] * B[3][3];

    let zx = A[2][0] * B[0][0] + A[2][1] * B[1][0] + A[2][2] * B[2][0] + A[2][3] * B[3][0];
    let zy = A[2][0] * B[0][1] + A[2][1] * B[1][1] + A[2][2] * B[2][1] + A[2][3] * B[3][1];
    let zz = A[2][0] * B[0][2] + A[2][1] * B[1][2] + A[2][2] * B[2][2] + A[2][3] * B[3][2];
    let zw = A[2][0] * B[0][3] + A[2][1] * B[1][3] + A[2][2] * B[2][3] + A[2][3] * B[3][3];

    let wx = A[3][0] * B[0][0] + A[3][1] * B[1][0] + A[3][2] * B[2][0] + A[3][3] * B[3][0];
    let wy = A[3][0] * B[0][1] + A[3][1] * B[1][1] + A[3][2] * B[2][1] + A[3][3] * B[3][1];
    let wz = A[3][0] * B[0][2] + A[3][1] * B[1][2] + A[3][2] * B[2][2] + A[3][3] * B[3][2];
    let ww = A[3][0] * B[0][3] + A[3][1] * B[1][3] + A[3][2] * B[2][3] + A[3][3] * B[3][3];

    return [[xx, xy, xz, xw],
            [yx, yy, yz, yw],
            [zx, zy, zz, zw],
            [wx, wy, wz, ww]];
}

function Print(A: Mat4) {
    for let i = 0; i < 4; i += 1 {
        for let j = 0; j < 4; j += 1 {
            printf("%10.4f  ", A[i][j] as f64);
        }
        printf("\n");
    }
}

function main() -> i32 {
    let A = [[ 1.0,  9.0,  3.0,  4.0],
             [ 2.0, -4.0,  5.0,  0.5],
             [-3.0,  1.0,  6.0, -3.0],
             [ 6.0, 10.0, 11.0,  1.0]];
    let B = MatMul(A, A);
    Print(A);
    Print(B);
    return 0;
}
```

```
// Structs and auto-deref

function printf(fmt: *i8, ...) -> i32;

type Vec3 = struct {
    x: f32,
    y: f32,
    z: f32,
};

function Print(v: Vec3) {
    let x = v.x as f64;
    let y = v.y as f64;
    let z = v.z as f64;
    printf("%f %f %f\n", x, y, z);
}

// Field accesses on pointers to structs are auto-deref'd.
function Dot(x: *Vec3, y: *Vec3) -> f32 {
    return x.x * y.x + x.y * y.y + x.z * y.z;
}

function main(argc: i32, argv: **i8) -> i32 {
    let a: Vec3 = { x: 1.0, y: 2.0, z: 3.0, };
    let b: Vec3 = { x: 2.0, y: 3.0, z: 4.0, };
    Print(a);
    Print(b);

    let ab = Dot(&a, &b);
    printf("dot a, b = %f\n", ab as f64);

    return 0;
}
```

```
// Pattern matching with `if let`

function printf(fmt: *i8, ...);

type Expr = enum {
    Int(i32),
    String(*i8),
};

function Print(e: Expr) {
    if let Int(i) = e {
        printf("%d\n", i);
    }
    if let String(s) = e {
        printf("%s\n", s);
    }
}

function main() -> i32 {
    let e = Expr.Int(1);
    Print(e);

    e = Expr.String("hello world");
    Print(e);

    return 0;
}
```
