use ::nix::unistd::{Uid, Gid};
use ::std::os::unix::io::AsRawFd;

pub fn fchown<T: AsRawFd>(stream: T, user: Uid, group: Gid)
                          -> Result<(), ::nix::Error> {
    let ret = unsafe {
        ::libc::fchown(stream.as_raw_fd(),
                       user.into(),
                       group.into())
    };

    if ret == 0 {
        Ok(())
    }
    else {
        Err(::nix::Error::Sys(::nix::errno::Errno::from_i32(ret)))
    }
}

pub fn fchmod<T: AsRawFd>(stream: T, mode: ::libc::mode_t)
                          -> Result<(), ::nix::Error> {
    let ret = unsafe {
        ::libc::fchmod(stream.as_raw_fd(), mode)
    };

    if ret == 0 {
        Ok(())
    }
    else {
        Err(::nix::Error::Sys(::nix::errno::Errno::from_i32(ret)))
    }
}
