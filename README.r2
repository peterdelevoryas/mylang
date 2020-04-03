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

fn main() {
    let x: i32;
}
