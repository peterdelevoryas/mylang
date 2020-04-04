fn printf(fmt: *i8, ...) -> i32;
fn opendir(filename: *i8) -> *DIR;
fn strlen(s: *i8) -> i64;
fn sprintf(s: *i8, fmt: *i8, ...) -> i32;
fn readdir(dirp: *DIR) -> *dirent;
fn realpath(file_name: *i8, resolved_name: *i8) -> *i8;
fn perror(s: *i8);
fn chdir(path: *i8) -> i32;
fn rename(old: *i8, new: *i8) -> i32;

type DIR struct {}
type dirent struct {
    d_ino: i64,
    d_seekoff: i64,
    d_reclen: i16,
    d_namlen: i16,
    d_type: i8,
    d_name: [1024]i8,
}

const DT_REG: i8 = 8;

fn print(entry: *dirent) {
    let d_name = &entry.d_name[0];
    printf("d_name = %p, %s\n", d_name, d_name);
}

fn main(argc: i32, argv: **i8) -> i32 {
    if argc < 2 {
        printf("usage: %s dir\n", argv[0]);
        return 1;
    }

    let dir_path = argv[1];
    let dir = opendir(dir_path);
    if dir == null {
        printf("unable to open current directory\n");
        return 1;
    }
    chdir(dir_path);

    for let entry = readdir(dir); entry != null; entry = readdir(dir) {
        if entry.d_type != DT_REG {
            continue;
        }
        let entry_name = &entry.d_name[0];
        let renamed: [1024]i8;
        sprintf(&renamed[0], "%s.r2", entry_name);
        let renamed = &renamed[0];

        printf("%-20s -> %-20s\n", entry_name, renamed);
        rename(entry_name, renamed);
    }

    return 0;
}
