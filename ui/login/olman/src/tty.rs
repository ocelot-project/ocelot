use ::libc;
use std::path::{Path, PathBuf};
use std::os::unix::io::AsRawFd;
use std::sync::Mutex;
use std::ffi::CStr;

lazy_static! {
    pub static ref STDIN_TTYNAME: Option<PathBuf> =
        ttyname(::std::io::stdout());
    static ref TTYNAME_MUTEX: Mutex<()> = Mutex::new(());
}

pub fn ttyname<T: AsRawFd>(stream: T) -> Option<PathBuf> {
    // TODO: use ttyname_r if portable
    let _mutex = TTYNAME_MUTEX.lock(); // MT-Unsafe race:ttyname
    let np = unsafe { libc::ttyname(stream.as_raw_fd()) };
    if np.is_null() {
        None
    }
    else {
        let name_str = unsafe { CStr::from_ptr(np) };
        let name = name_str.to_string_lossy().into_owned();
        Some(PathBuf::from(name))
    }
}

pub fn is_my_tty<S>(tty: S) -> bool where S: Into<String> {
    let mut tty_path = PathBuf::from(tty.into());
    if !tty_path.is_absolute() {
        tty_path = Path::new("/dev").join(tty_path)
    }

    match *STDIN_TTYNAME {
        Some(ref st) => tty_path == *st,
        None => false
    }
}
