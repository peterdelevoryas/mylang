fn printf(fmt: *i8, ...);

type vec3 struct {
    x: f32,
    y: f32,
    z: f32,
}

fn dot(x: vec3, y: vec3) -> f32 {
    return x.x * y.x + x.y * y.y + x.z * y.z;
}

fn main() -> i32 {
    let x: vec3 = { x: 1.0, y: 2.0, z: 3.0, };
    let y: vec3 = { x: 7.0, y: 8.0, z: 9.0, };
    let p = dot(x, y);
    // check: 50.000000
    printf("%f\n", p as f64);
    return 0;
}
