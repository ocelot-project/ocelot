extern crate libc;
#[macro_use]
extern crate lazy_static;
extern crate num_traits;
#[macro_use]
extern crate num_derive;

use libc::{utmpx, getutxent, pututxline, setutxent, endutxent};
use std::marker::PhantomData;
use std::cell::Cell;
use std::sync::Mutex;
use std::ffi::CStr;
use num_traits::cast::FromPrimitive;

// TODO: work around MT-Unsafe sig:ALRM timer

#[derive(FromPrimitive, PartialEq, Debug)]
pub enum UTMPXType {
    Empty = 0,
    RunLevel = 1,
    BootTime = 2,
    NewTime = 3,
    OldTime = 4,
    InitProcess = 5,
    LoginProcess = 6,
    UserProcess = 7,
    DeadProcess = 8,
    Accounting = 9,
    Unknown
}

#[derive(PartialEq)]
pub enum UTMPXState {
    Beginning,
    Middle,
    End
}

lazy_static! {
    pub static ref SYSTEM_UTMPX: Mutex<UTMPXFile> =
        Mutex::new(UTMPXFile::new());
}

// UTMPXEntry and to_str implementations thanks to nix UtsName
#[repr(C)]
#[derive(Clone, Copy)]
pub struct UTMPXEntry(pub utmpx);

impl UTMPXEntry {
    pub fn entry_type(&self) -> UTMPXType {
        UTMPXType::from_u32(self.0.ut_type as u32).unwrap_or(UTMPXType::Unknown)
    }

    pub fn pid(&self) -> libc::pid_t {
        self.0.ut_pid
    }

    pub fn line(&self) -> &str {
        to_str(&(&self.0.ut_line as *const libc::c_char )
               as *const *const libc::c_char)
    }

    pub fn id(&self) -> &str {
        to_str(&(&self.0.ut_id as *const libc::c_char )
               as *const *const libc::c_char)
    }

    pub fn user(&self) -> &str {
        to_str(&(&self.0.ut_user as *const libc::c_char )
               as *const *const libc::c_char)
    }

    pub fn host(&self) -> &str {
        to_str(&(&self.0.ut_host as *const libc::c_char )
               as *const *const libc::c_char)
    }

    pub fn exit_status(&self) -> (libc::c_short, libc::c_short) {
        (self.0.ut_exit.e_termination,
         self.0.ut_exit.e_exit)
    }

    pub fn session(&self) -> libc::int32_t {
        self.0.ut_session
    }

    pub fn time(&self) -> (libc::int32_t, libc::int32_t) {
        (self.0.ut_tv.tv_sec,
         self.0.ut_tv.tv_usec)
    }

    pub fn ipv6_addr(&self) -> &[libc::int32_t] {
        &self.0.ut_addr_v6[..]
    }
}

#[inline]
fn to_str<'a>(s: *const *const libc::c_char) -> &'a str {
    unsafe {
        let res = CStr::from_ptr(*s).to_bytes();
        std::str::from_utf8_unchecked(res)
    }
}

pub struct UTMPXFile {
    pub state: UTMPXState,
    _token: PhantomData<Cell<()>> // MT-Unsafe race:utent race:utentbuf
}

impl UTMPXFile {
    fn new() -> Self {
        let mut n = Self {
            state: UTMPXState::Beginning,
            _token: PhantomData
        };

        n.reset();
        n
    }

    pub fn reset(&mut self) {
        unsafe {
            setutxent();
        }

        self.state = UTMPXState::Beginning;
    }

    pub fn iter(&mut self) -> UTMPXIterator {
        UTMPXIterator::new(self)
    }

    pub fn write_entry(&mut self, entry: UTMPXEntry) -> Result<(), ()> {
        self.reset();

        let res = unsafe {
            pututxline(&entry.0)
        };

        self.state = UTMPXState::Middle;

        if res.is_null() {
            Err(())
        }
        else {
            Ok(())
        }
    }
}

impl Drop for UTMPXFile {
    fn drop(&mut self) {
        unsafe {
            endutxent();
        }
    }
}

pub struct UTMPXIterator<'a> {
    obj: &'a mut UTMPXFile
}

impl<'a> UTMPXIterator<'a> {
    fn new(obj: &'a mut UTMPXFile) -> Self {
        if obj.state != UTMPXState::Beginning {
            obj.reset();
        }

        Self {
            obj: obj
        }
    }
}

impl<'a> Iterator for UTMPXIterator<'a> {
    type Item = UTMPXEntry;

    fn next(&mut self) -> Option<Self::Item> {
        unsafe {
            let utxent = getutxent();
            if utxent.is_null() {
                self.obj.state = UTMPXState::End;
                None
            }
            else {
                self.obj.state = UTMPXState::Middle;
                Some(UTMPXEntry(*utxent.clone()))
            }
        }
    }
}
