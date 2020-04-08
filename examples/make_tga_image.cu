fn printf(fmt: *i8, ...);
fn malloc(size: i64) -> *i8;
fn memset(b: *i8, c: i32, len: i64) -> *i8;
fn memcpy(dst: *i8, src: *i8, n: i64) -> *i8;
fn free(ptr: *i8);
type FILE = ();
fn fopen(path: *i8, mode: *i8) -> *FILE;
fn fwrite(ptr: *i8, size: i64, nitems: i64, stream: *FILE) -> i64;
fn fclose(stream: *FILE) -> i32;

type point struct {
    x: i32,
    y: i32,
}

type pixel struct {
    b: i8,
    g: i8,
    r: i8,
}

type image struct {
    pixels: *pixel,
    width: i32,
    height: i32,
}

fn new_image(width: i32, height: i32) -> image {
    let size = width as i64 * height as i64 * sizeof(pixel);
    let pixels = malloc(size);
    memset(pixels, 0, size);
    let pixels = pixels as *pixel;
    return {
        pixels: pixels,
        width: width,
        height: height,
    };
}

fn set_pixel(image: image, p: point, color: pixel) {
    if p.x >= image.width {
        return;
    }
    if p.y >= image.height {
        return;
    }
    image.pixels[image.width * p.y + p.x] = color;
}

fn abs(x: i32) -> i32 {
    if x < 0 {
        return -1 * x;
    }
    return x;
}

fn swap_i32(x: *i32, y: *i32) {
    let tmp = *x;
    *x = *y;
    *y = tmp;
}

fn swap_points(x: *point, y: *point) {
    let tmp = *x;
    *x = *y;
    *y = tmp;
}

fn draw_line(image: image, a: point, b: point, color: pixel) {
    let steep = false;
    if abs(a.x - b.x) < abs(a.y - b.y) {
        swap_i32(&a.x, &a.y);
        swap_i32(&b.x, &b.y);
        steep = true;
    }
    if a.x > b.x {
        swap_points(&a, &b);
    }
    let m = (b.y - a.y) as f32 / (b.x - a.x) as f32;
    for let x = a.x; x <= b.x; x += 1 {
        let dx = (x - a.x) as f32;
        let y = m * dx + a.y as f32;
        let p: point = { x: x, y: y as i32 };
        if steep {
            swap_i32(&p.x, &p.y);
        }
        set_pixel(image, p, color);
    }
}

fn draw_grid(image: image) {
    let spacing = 50;
    for let x = spacing; x < image.width; x += spacing {
        for let y = spacing; y < image.height; y += spacing {
            draw_line(image, { x: x, y: 0 }, { x: x, y: image.height }, { b: 0, g: 0, r: 255 });
            draw_line(image, { x: 0, y: y }, { x: image.width, y: y }, { b: 0, g: 0, r: 255 });
        }
    }
}

fn flip_image(image: image) {
    let row_size = image.width as i64 * sizeof(pixel);
    let tmp = malloc(row_size);

    for let row = 0; row < image.height / 2; row += 1 {
        let top_row = &image.pixels[row * image.width] as *i8;
        let bottom_row = &image.pixels[(image.height - 1 - row) * image.width] as *i8;
        memcpy(tmp, top_row, row_size);
        memcpy(top_row, bottom_row, row_size);
        memcpy(bottom_row, tmp, row_size);
    }

    free(tmp);
}

fn split_i16(x: i16) -> (i8, i8)  {
    let lower = ((x >> 0) & 255) as i8;
    let upper = ((x >> 8) & 255) as i8;
    return (lower, upper);
}

fn write_tga_file(image: image, path: *i8) {
    let pixels_size = image.width as i64 * image.height as i64 * sizeof(pixel);
    let header_size: i64 = 18;
    let output_size = header_size + pixels_size;
    let output = malloc(output_size);

    memset(output, 0, header_size);
    output[2] = 2;
    let (x, y) = split_i16(image.width as i16);
    output[12] = x;
    output[13] = y;
    let (x, y) = split_i16(image.height as i16);
    output[14] = x;
    output[15] = y;
    output[16] = (sizeof(pixel) * 8) as i8;
    output[17] = 32;

    memcpy(output + header_size as i32, image.pixels as *i8, pixels_size);

    let f = fopen(path, "w");
    fwrite(output, 1, output_size, f);
    fclose(f);
    free(output);
}

fn main() {
    let image = new_image(200, 200);
    draw_grid(image);
    flip_image(image);
    write_tga_file(image, "output.tga");
}
