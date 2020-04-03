fn printf(fmt: *i8, ...);

type ino_t = i64;
type off_t = i64;
type ushort = i16;
type uchar = i8;

type dirent struct {
    d_ino: ino_t,
    d_off: off_t,
    d_reclen: ushort,
    d_type: uchar,
}

fn mod(a: i32, n: i32) -> i32 {
    return a - n * (a / n);
}

fn main() {
    let x: [256]i8;
    for let i = 0; i < 256; i += 1 {
        x[i] = i as i8;
    }
    for let i = 0; i < 256; i += 1 {
        if mod(i, 16) == 0 {
            printf("\n");
        }
        printf("0x%02x ", x[i]);
    }
    printf("\n");
}
