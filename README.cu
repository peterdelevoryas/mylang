type string = *i8;

fn printf(fmt: string, ...) -> i32;
fn sprintf(s: string, fmt: string, ...) -> i32;
fn strlen(s: string) -> i64;
fn opendir(filename: string) -> *DIR;
fn readdir(dir: *DIR) -> *dirent;
fn rename(old: string, new: string) -> i32;
fn perror(s: string);
fn strstr(haystack: string, needle: string) -> string;

const DT_REG: i8 = 8;

type DIR struct {}
type dirent struct {
    d_ino: i64,
    d_seekoff: i64,
    d_reclen: i16,
    d_namlen: i16,
    d_type: i8,
    d_name: [1024]i8,
}

fn main() -> i32 {
    let dir = opendir(".");
    if dir == null {
        perror("opendir");
        return 1;
    }

    for let entry = readdir(dir); entry != null; entry = readdir(dir) {
        if entry.d_type != DT_REG {
            continue;
        }
        let name = &entry.d_name[0];
        let extension = strstr(name, ".r2");
        if extension == null {
            continue;
        }
        let renamed: [1024]i8;
        let renamed = &renamed[0];
        sprintf(renamed, "%.*s.cu", extension - name, name);
        printf("rename %s %s\n", name, renamed);
        rename(name, renamed);
    }

    return 0;
}
