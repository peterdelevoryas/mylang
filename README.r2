fn printf(fmt: *i8, ...) -> i32;
fn opendir(filename: *i8) -> *DIR;
fn strlen(s: *i8) -> i64;
fn sprintf(s: *i8, fmt: *i8, ...) -> i32;
fn readdir(dirp: *DIR) -> *dirent;

type DIR struct {}
type dirent struct {
    d_ino: i64,
    d_seekoff: i64,
    d_reclen: i16,
    d_namlen: i16,
    d_type: i8,
    d_name: [1024]i8,
}

fn print(entry: *dirent) {
    let d_name = &entry.d_name[0];
    printf("d_name = %p, %s\n", d_name, d_name);
}

fn main() -> i32 {
    let dir = opendir(".");
    if dir == null {
        printf("unable to open current directory\n");
        return 1;
    }

    for let entry = readdir(dir); entry != null; entry = readdir(dir) {
        printf("%s\n", &entry.d_name[0]);
    }

    return 0;
}
