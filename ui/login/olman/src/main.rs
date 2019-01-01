#[macro_use]
extern crate lazy_static;
extern crate users;
extern crate getopts;
extern crate nix;
extern crate utmpx;
extern crate libc;
#[macro_use]
extern crate slog;
extern crate slog_syslog;
extern crate regex;
extern crate pam_sys;
extern crate pwd;
extern crate toml;
#[macro_use]
extern crate serde_derive;

mod tty;
mod credentials;
mod pam;
mod fperms;
mod config;

use OlmanError::*;
use nix::unistd::{isatty, setuid, setgid, initgroups, Uid, Gid, Pid};
use std::os::unix::io::AsRawFd;
use utmpx::{UTMPXEntry, UTMPXType, SYSTEM_UTMPX};
use slog::Drain;
use std::collections::HashMap;
use std::env;
use std::process::Command;
use std::os::unix::process::CommandExt;
use std::error::Error;
use std::sync::Mutex;
use credentials::Credentials;
use regex::Regex;
use pam::{PamSession, PamStringItem, AuthStatus};
use config::Config;

#[derive(Debug)]
pub enum OlmanError {
    Usage,
    EffectiveRoot,
    NoTerminal,
    NoTtyname,
    NoUtmpxEntry,
    NoUser(String),
    NoGroup(String),
    SpawnError(String),
    PamInitError,
    PamError(String),
    PamAbort,
    TooManyLogins(usize, String, String),
    AuthTokenExpired,
    CannotFindUser(String),
    BadGroupID(Gid, String),
    InitgroupsFailed(String),
    TtyOwnership(String, String),
    ShellError(String)
}

static DEFAULT_PROGNAME: &'static str = "olman";

lazy_static! {
    static ref PROG_PATH: std::path::PathBuf =
        ::std::env::current_exe() .unwrap_or(std::path::PathBuf::new());

    static ref PROG_NAME: &'static str = {
        match PROG_PATH.file_name() {
            Some(progname) => progname.to_str().unwrap_or(DEFAULT_PROGNAME),
            None => DEFAULT_PROGNAME
        }
    };

    static ref AS_ROOT: bool = Uid::current().is_root();

    static ref AUTH_STORE: Mutex<Credentials> =
        Mutex::new(Credentials::new());

    static ref USER_RE: Regex = Regex::new(r"login: (.*)").unwrap();
    static ref PASSWORD_RE: Regex = Regex::new(r"password: (.*)").unwrap();
    static ref SESSION_RE: Regex = Regex::new(r"session: (.*)").unwrap();
}

fn usage() {
    eprintln!("Usage: {} [-p] frontend [args...]", *PROG_NAME);
}

fn session_utmpx() -> Result<UTMPXEntry, OlmanError> {
    match *tty::STDIN_TTYNAME {
        Some(ref _ttyname) => {
            let mut utmp = SYSTEM_UTMPX.lock().unwrap();
            utmp.iter()
                .find(|entry| Pid::from_raw(entry.pid()) == Pid::this() &&
                      entry.id() != "" &&
                      (entry.entry_type() == UTMPXType::LoginProcess ||
                       entry.entry_type() == UTMPXType::UserProcess) &&
                      tty::is_my_tty(entry.line()))
                .ok_or(NoUtmpxEntry)
        },
        None => Err(NoTtyname)
    }
}

fn update_utmpx<S>(name: S,
                   line: S,
                   host: S,
                   utmp: &mut UTMPXEntry)
where S: Into<String> {
    let h = host.into();
    let hostname = if h != "" {
        h
    }
    else {
        utmp.host().into()
    };
    let new_line = line.into().replacen("/dev/", "", 1);
    let epoch_time = std::time::SystemTime::now()
        .duration_since(std::time::UNIX_EPOCH).unwrap();
    utmp.0.ut_type = libc::USER_PROCESS;
    utmp.0.ut_pid = Pid::this().into();
    let mut new_line_bytes = std::ffi::CString::new(new_line)
        .unwrap()
        .into_bytes_with_nul()
        .into_iter()
        .map(|c| c as libc::c_char)
        .collect::<Vec<i8>>();
    new_line_bytes.resize(32, 0);
    new_line_bytes[31] = 0;
    utmp.0.ut_line.copy_from_slice(new_line_bytes.as_slice());
    let mut name_bytes = std::ffi::CString::new(name.into())
        .unwrap()
        .into_bytes_with_nul()
        .into_iter()
        .map(|c| c as libc::c_char)
        .collect::<Vec<i8>>();
    name_bytes.resize(32, 0);
    name_bytes[31] = 0;
    utmp.0.ut_user.copy_from_slice(name_bytes.as_slice());
    let mut hostname_bytes = std::ffi::CString::new(hostname)
        .unwrap()
        .into_bytes_with_nul()
        .into_iter()
        .map(|c| c as libc::c_char)
        .collect::<Vec<i8>>();
    hostname_bytes.resize(256, 0);
    hostname_bytes[255] = 0;
    utmp.0.ut_host.copy_from_slice(hostname_bytes.as_slice());
    utmp.0.ut_exit = libc::__exit_status {
        e_termination: 0,
        e_exit: 0
    };
    utmp.0.ut_session = nix::sys::termios::tcgetsid(
        std::io::stdin().as_raw_fd())
        .unwrap_or(Pid::from_raw(-1))
        .into();
    utmp.0.ut_tv = libc::__timeval {
        tv_sec: epoch_time.as_secs() as i32,
        tv_usec: (epoch_time.as_secs() * 1000 +
                  epoch_time.subsec_nanos() as u64 / 1_000_000) as i32
    };
    // TODO: log ut_addr_v6
}

fn olman() -> Result<(), OlmanError> {
    // Check for effective root
    if !Uid::effective().is_root() {
        return Err(EffectiveRoot);
    }

    // Parse command line arguments
    let args: Vec<String> = std::env::args().collect();
    let mut opts = getopts::Options::new();
    opts.optflag("p", "", "");
    let cli = match opts.parse(&args[1..]) {
        Ok(c) => c,
        Err(e) => {
            panic!(e.to_string())
        }
    };

    let preserve_env = cli.opt_present("p");
    let frontend_cmd = if cli.free.is_empty() {
        "".into()
    }
    else {
        cli.free[0].clone()
    };
    if frontend_cmd == "" {
        return Err(Usage);
    }
    let frontend_args = cli.free[1..].to_vec();

    // Ensure we're talking directly to a terminal
    if !(isatty(std::io::stdout().as_raw_fd()).unwrap_or(false)) ||
        !(isatty(std::io::stderr().as_raw_fd()).unwrap_or(false)) ||
        !(isatty(std::io::stdin().as_raw_fd()).unwrap_or(false)) {
            return Err(NoTerminal);
        }

    // Try to find a utmpx entry for the current user session.
    // If we're really running as root (not setuid), allow logins
    // anyway just in case utmpx has been corrupted.
    let utmpx_entry = session_utmpx();
    match utmpx_entry {
        Err(NoUtmpxEntry) if *AS_ROOT => (),
        Err(e) => return Err(e),
        _ => ()
    }

    let olman_toml: Config =
        toml::from_str(&std::fs::read_to_string("/etc/olman.toml")
                       .unwrap_or("".into())).unwrap_or_default();

    let _log_root = slog::Logger::root(
        slog_syslog::unix_3164(
            slog_syslog::Facility::LOG_AUTHPRIV
        ).unwrap().ignore_res(),
        o!());

    let current_tty_name = tty::ttyname(std::io::stdin());
    let current_tty = current_tty_name
        .as_ref()
        .map(|n| n.to_string_lossy())
        .unwrap_or("UNKNOWN".into());
    let hostname = utmpx_entry.as_ref()
        .map(|e| e.host())
        .unwrap_or("");
    let vt_num = current_tty.replace("/dev/tty", "");

    let mut new_env = HashMap::new();
    if preserve_env {
        for (key, value) in env::vars() {
            new_env.insert(key, value);
        }
    }
    else if let Ok(term) = env::var("TERM"){
        // preserve TERM from getty
        new_env.insert("TERM".into(), term);
    }
    let _ = env::var("LANG")
        .and_then(|v| Ok(new_env.insert("LANG".into(), v)));
    // Add the timezone environment variable so that time functions work
    // correctly
    let _ = env::var("TZ")
        .and_then(|v| Ok(new_env.insert("TZ".into(), v)));
    // Add the clock frequency so that profiling commands work correctly
    let _ = env::var("HZ")
        .and_then(|v| Ok(new_env.insert("HZ".into(), v)));
    new_env.insert("OLMAN_TTY".into(), current_tty.replace("/dev/", ""));
    new_env.insert("OLMAN_VT".into(), vt_num.clone());
    new_env.insert("OLMAN_X_DISPLAY".into(), format!(":{}", vt_num));

    // Set up the new environment
    for (key, _) in env::vars() {
        if !new_env.contains_key(&key) {
            env::remove_var(key);
        }
    }
    for (key, value) in &new_env {
        env::set_var(key, value);
    }

    // The origin is a string representing the requesting TTY and
    // hostname
    let origin = if hostname != "" {
        format!(" on '{:.100}' from '{:.200}'",
                current_tty,
                hostname)
    }
    else {
        format!(" on '{:.100}'", current_tty)
    };

    let frontend_username = olman_toml.frontend.user;
    let frontend_groupname = olman_toml.frontend.group;
    let frontend_user = users::get_user_by_name(&frontend_username)
        .ok_or(OlmanError::NoUser(frontend_username.clone()))?.uid();
    let frontend_group = users::get_group_by_name(&frontend_groupname)
        .ok_or(OlmanError::NoGroup(frontend_groupname.clone()))?.gid();

    // TODO: get the session list from a configuration file
    let session_list = olman_toml.session;
    let mut session_args = vec![];
    for session in session_list.iter() {
        session_args.push("-s");
        session_args.push(&session.name);
    }

    // TODO: get FAIL_DELAY and LOGIN_RETRIES from login.defs
    let delay = 1;
    let retries = 3;
    let mut login_attempts = 0;
    let mut last_login_error = "";

    loop {
        let mut current_username = String::new();
        let mut current_session = 0;
        let auth_username = frontend_username.clone();
        login_attempts = login_attempts + 1;
        let error_arg = if last_login_error == "" {
            vec![]
        }
        else {
            vec!["-e", last_login_error]
        };

        // TODO: use own PAM service name
        let mut pam_session = PamSession::new("login",
                                              Some(pam::pam_conversation))?;
        pam_session.set_string_item(PamStringItem::RequestingHost,
                                    hostname)?;
        pam_session.set_string_item(PamStringItem::TTY,
                                    current_tty.clone())?;
        pam_session.set_fail_delay(1000000 * delay)?;

        let mut auth_store = AUTH_STORE.lock().unwrap();
        auth_store.set_credentials(|| {
            let frontend = Command::new(frontend_cmd.clone())
                .args(frontend_args.clone())
                .args(session_args.clone())
                .args(error_arg)
                .env("HOME", "/tmp")
                .current_dir("/tmp")
                .stdin(std::process::Stdio::inherit())
                .stderr(std::process::Stdio::inherit())
                .before_exec(move || {
                    let gid = Gid::from_raw(frontend_group);
                    let uid = Uid::from_raw(frontend_user);

                    // TODO: report Rust libstd bug --
                    // Command with .uid or .gid should fchown its pipes
                    let _ = fperms::fchown(std::io::stdout(),
                                           uid,
                                           gid);

                    // Note: we have to execute setgid and then setuid; once
                    // we setuid, we'll no longer have permission to setgid
                    // freely
                    let username = std::ffi::CString::new(auth_username.clone())
                        .unwrap();
                    initgroups(&username, gid)
                        .and(setgid(gid))
                        .and(setuid(uid))
                        .map_err(|e| {
                            std::io::Error::new(
                                std::io::ErrorKind::Other,
                                e.description())
                        })
                })
                .output()
                .map_err(|e| {
                    OlmanError::SpawnError(e.description().into())
                })?;
            let username: String =
                USER_RE.captures(&String::from_utf8_lossy(&frontend.stdout))
                .and_then(|c| {
                    c.get(1).map(|m| m.as_str().into())
                })
                .unwrap_or("".into());
            let password =
                PASSWORD_RE.captures(&String::from_utf8_lossy(&frontend.stdout))
                .and_then(|c| {
                    c.get(1).map(|m| m.as_str().into())
                })
                .unwrap_or("".into());
            let session =
                SESSION_RE.captures(&String::from_utf8_lossy(&frontend.stdout))
                .and_then(|c| {
                    c.get(1).map(|m| m.as_str())
                })
                .unwrap_or("0")
                .parse().unwrap_or(0);

            pam_session.set_string_item(PamStringItem::User,
                                        username.clone())?;
            current_username = username.clone();
            current_session = session;
            last_login_error = "";

            Ok((username, password, session))
        })?;
        drop(auth_store);

        match pam_session.authenticate() {
            AuthStatus::Authenticated => {
                match pam_session.validate_account() {
                    Err(OlmanError::AuthTokenExpired) =>
                        pam_session.change_expired_token()?,
                    Err(e) => return Err(e),
                    _ => {}
                };

                let _ = pam_session.get_string_item(PamStringItem::User);
                // TODO: handle hushed users
                pam_session.open_session(false)?;

                // Get the username from PAM one more time
                let username =
                    pam_session.get_string_item(PamStringItem::User)
                    .unwrap_or(current_username);
                let passwd = pwd::Passwd::from_name(&username)
                    .unwrap()
                    .ok_or(OlmanError::CannotFindUser(username.clone()))?;
                let passwd_uid = Uid::from_raw(passwd.uid);
                let passwd_dir = passwd.dir.clone();
                let passwd_shell = passwd.shell.clone();
                let passwd_name = passwd.name.clone();

                // Initialize the authenticated user's primary group ID and
                // supplementary group IDs.
                setgid(Gid::from_raw(passwd.gid))
                    .map_err(|_e| OlmanError::BadGroupID(
                        Gid::from_raw(passwd.gid),
                        passwd.name.clone()))?;
                initgroups(&std::ffi::CString::new(
                    passwd.name.clone()).unwrap(),
                           Gid::from_raw(passwd.gid))
                    .map_err(|_e| OlmanError::InitgroupsFailed(
                        passwd.name.clone()))?;

                pam_session.establish_credentials()?;

                // Shell field separators are a security risk, so make sure
                // the user inherits a safe value.
                if env::var("IFS").is_ok() {
                    env::set_var("ITS", " \t\n");
                }

                // Reconfigure the login TTY to be owned by the authenticated
                // user.
                // TODO: get the TTY group ID and permissions from login.defs
                fperms::fchown(std::io::stdin(),
                               Uid::from_raw(passwd.uid),
                               Gid::from_raw(passwd.gid))
                    .and(fperms::fchmod(std::io::stdin(), 0o600))
                    .map_err(|e|
                             OlmanError::TtyOwnership(e.description().into(),
                                                      passwd.name.clone()))?;

                let _ = unsafe {
                    nix::sys::signal::sigaction(
                        nix::sys::signal::Signal::SIGINT,
                        &nix::sys::signal::SigAction::new(
                            nix::sys::signal::SigHandler::SigIgn,
                            nix::sys::signal::SaFlags::empty(),
                            nix::sys::signal::SigSet::empty()))
                };

                // Spawn a new process for the authenticated user's session.
                let session = session_list.to_vec().into_iter()
                    .nth(current_session as usize)
                    .unwrap_or(config::Session {
                        name: "Default Shell".into(),
                        ..Default::default()
                    });
                let mut session_shell = session.shell.clone()
                    .unwrap_or(olman_toml.olman.session_default_shell);
                let command_flag = olman_toml.olman.command_flag.clone();
                let graphical_command = olman_toml.graphical.command.clone();
                env::set_var("OLMAN_SESSION",
                             session.command.clone().unwrap_or("".into()));
                let mut session_proc = match session.command.clone() {
                    Some(command) => {
                        let mut c = Command::new(session_shell.clone());
                        c.args(&["-l",
                                 &command_flag]);
                        if session.graphical {
                            c.arg(olman_toml.graphical.command);
                        }
                        else {
                            c.arg(&command);
                        }
                        c
                    },
                    None => {
                        session_shell = session.shell.clone()
                            .unwrap_or(passwd.shell);
                        let mut c = Command::new(session_shell.clone());
                        c.args(&["-l"]);
                        if session.graphical {
                            c.args(&[&command_flag,
                                     &olman_toml.graphical.command]);
                        }
                        c
                    }
                };

                // Update utmp with the logged in user's session.
                // If we don't have a utmpx entry, it's probably
                // corrupted, so don't bother updating it.
                // TODO: see if this needs to happen after setsid and
                // TIOCSCTTY
                if let Ok(mut utmpx) = session_utmpx() {
                    update_utmpx(username,
                                 current_tty.into(),
                                 hostname.into(),
                                 &mut utmpx);
                    let _ = SYSTEM_UTMPX.lock().unwrap()
                        .write_entry(utmpx);
                }

                session_proc
                    .before_exec(move || {
                        // If we were launched by init, start a new session
                        if nix::unistd::getppid() == Pid::from_raw(1) {
                            eprintln!("warning: TIOCSCTTY not implemented");
                            let _ = nix::unistd::setsid();
                            // TODO: call TIOCSCTTY ioctl
                        }

                        // TODO: make sure all open files are closed
                        // (check by spawning ls /proc/self/fd)

                        // Drop root privileges
                        setuid(passwd_uid).map_err(|e| {
                            std::io::Error::new(
                                std::io::ErrorKind::Other,
                                e.description())
                        })?;

                        // Change directory to HOME and set environment
                        // variables
                        if env::set_current_dir(&passwd_dir).is_err() {
                            // TODO: get DEFAULT_HOME from login.defs
                            env::set_current_dir("/")?;
                            eprintln!(
                                "{}: No home directory, logging in with HOME=/",
                                *PROG_NAME);
                            env::set_var("HOME", "/");
                        }
                        else {
                            env::set_var("HOME", passwd_dir.as_str());
                        }

                        env::set_var("SHELL", passwd_shell.as_str());
                        env::set_var("USER", // BSD
                                       passwd_name.as_str());
                        env::set_var("LOGNAME", // Other POSIX
                                       passwd_name.as_str());
                        // TODO: take ENV_SUPATH and ENV_PATH from login.defs
                        env::set_var("PATH", "/sbin:/bin:/usr/sbin:/usr/bin");
                        // TODO: set up MAIL environment variable
                        // TODO: use ENVIRON_FILE from login.defs

                        let pam_env = pam_session
                            .get_env()
                            .map(|mut e| e.to_map())
                            .unwrap_or(HashMap::new());
                        for (key, value) in pam_env {
                            env::set_var(key, value);
                        }

	                      // (void) setlocale (LC_ALL, "");
	                      // (void) bindtextdomain (PACKAGE, LOCALEDIR);
	                      // (void) textdomain (PACKAGE);

                        // TODO: handle hushed logins
                        env::set_var("HUSHLOGIN", "FALSE");

                        // TODO: parse TTYTYPE_FILE from login.defs if
                        // the environment variable TERM isn't set

                        let _ = unsafe {
                            nix::sys::signal::sigaction(
                                nix::sys::signal::Signal::SIGQUIT,
                                &nix::sys::signal::SigAction::new(
                                    nix::sys::signal::SigHandler::SigDfl,
                                    nix::sys::signal::SaFlags::empty(),
                                    nix::sys::signal::SigSet::empty()))
                        };
                        let _ = unsafe {
                            nix::sys::signal::sigaction(
                                nix::sys::signal::Signal::SIGTERM,
                                &nix::sys::signal::SigAction::new(
                                    nix::sys::signal::SigHandler::SigDfl,
                                    nix::sys::signal::SaFlags::empty(),
                                    nix::sys::signal::SigSet::empty()))
                        };
                        let _ = unsafe {
                            nix::sys::signal::sigaction(
                                nix::sys::signal::Signal::SIGALRM,
                                &nix::sys::signal::SigAction::new(
                                    nix::sys::signal::SigHandler::SigDfl,
                                    nix::sys::signal::SaFlags::empty(),
                                    nix::sys::signal::SigSet::empty()))
                        };
                        let _ = unsafe {
                            nix::sys::signal::sigaction(
                                nix::sys::signal::Signal::SIGHUP,
                                &nix::sys::signal::SigAction::new(
                                    nix::sys::signal::SigHandler::SigDfl,
                                    nix::sys::signal::SaFlags::empty(),
                                    nix::sys::signal::SigSet::empty()))
                        };
                        let _ = unsafe {
                            nix::sys::signal::sigaction(
                                nix::sys::signal::Signal::SIGINT,
                                &nix::sys::signal::SigAction::new(
                                    nix::sys::signal::SigHandler::SigDfl,
                                    nix::sys::signal::SaFlags::empty(),
                                    nix::sys::signal::SigSet::empty()))
                        };

                        // TODO: log successful login

                        // exec our child process. If successful, this doesn't
                        // return, and Command's exec is skipped. This means
                        // that the image name and arguments provided to Command
                        // are skipped. Session shells are execed with a dash
                        // as the first character of their argv[0], as a Unix
                        // convention signaling that the shell should run as a
                        // login shell.
                        // TODO: make a libstd feature request to be able to
                        // set argv[0].
                        match session.clone().command {
                            Some(command) => {
                                let cmd = if session.graphical {
                                    graphical_command.clone()
                                }
                                else {
                                    command
                                };
                                let argv = &[
                                    std::ffi::CString::new(
                                        format!("-{}",
                                                std::path::Path::new(
                                                    &session_shell.clone())
                                                .file_name()
                                                .unwrap()
                                                .to_string_lossy()))
                                        .unwrap(),
                                    std::ffi::CString::new(
                                        command_flag.clone())
                                        .unwrap(),
                                    std::ffi::CString::new(cmd)
                                        .unwrap()
                                ];

                                nix::unistd::execv(
                                    &std::ffi::CString::new(
                                        session_shell.clone())
                                        .unwrap(),
                                    argv)
                                    .map(|_| ())
                                    .map_err(|_|
                                             std::io::Error::last_os_error())
                            },
                            None => {
                                if session.graphical {
                                    let argv = &[
                                        std::ffi::CString::new(
                                            format!("-{}",
                                                    std::path::Path::new(
                                                        &session_shell.clone())
                                                    .file_name()
                                                    .unwrap()
                                                    .to_string_lossy()))
                                            .unwrap(),
                                        std::ffi::CString::new(
                                            command_flag.clone())
                                            .unwrap(),
                                        std::ffi::CString::new(
                                            graphical_command.clone())
                                            .unwrap()

                                    ];

                                    nix::unistd::execv(
                                        &std::ffi::CString::new(
                                            session_shell.clone())
                                            .unwrap(),
                                        argv)
                                        .map(|_| ())
                                        .map_err(|_|
                                                 std::io::Error::last_os_error())
                                }
                                else {
                                    let argv = &[
                                        std::ffi::CString::new(
                                            format!("-{}",
                                                    std::path::Path::new(
                                                        &session_shell.clone())
                                                    .file_name()
                                                    .unwrap()
                                                    .to_string_lossy()))
                                            .unwrap()
                                    ];

                                    nix::unistd::execv(
                                        &std::ffi::CString::new(
                                            session_shell.clone())
                                            .unwrap(),
                                        argv)
                                        .map(|_| ())
                                        .map_err(|_|
                                                 std::io::Error::last_os_error())
                                }
                            }
                        }
                    })
                    .status()
                    .map_err(|e|
                             OlmanError::ShellError(e.description().into()))?;

                return Ok(())
            },
            AuthStatus::MaxTriesExceeded => return
                Err(OlmanError::TooManyLogins(login_attempts,
                                              origin,
                                              current_username)),
            AuthStatus::Abort => return Err(OlmanError::PamAbort),
            _ => {
                // TODO: log here
                last_login_error = "Login incorrect";

                if login_attempts >= retries {
                    return Err(OlmanError::TooManyLogins(login_attempts,
                                                         origin,
                                                         current_username))
                }
            }
        }
    }
}

fn main() {
    ::std::process::exit(match olman() {
        Ok(_) => 0,
        Err(Usage) => {
            usage();
            1
        },
        Err(EffectiveRoot) => {
            eprintln!("{}: Cannot possibly work without effective root",
                      *PROG_NAME);
            1
        },
        Err(NoTerminal) => 1,
        Err(NoTtyname) => {
            eprintln!("{}: Unable to determine your tty name.", *PROG_NAME);
            1
        },
        Err(NoUtmpxEntry) => {
            eprintln!(
                "No utmp entry. You must exec \"{}\" from the lowest level \"sh\"",
                *PROG_NAME);
            1
        },
        Err(NoUser(u)) => {
            eprintln!("{}: Unable to find \"{}\" in the user database.",
                      *PROG_NAME,
                      u);
            // TODO: log here
            1
        },
        Err(NoGroup(g)) => {
            eprintln!("{}: Unable to find \"{}\" in the group database.",
                      *PROG_NAME,
                      g);
            // TODO: log here
            1
        },
        Err(SpawnError(e)) => {
            eprintln!("{}: Error spawning frontend process: {}",
                      *PROG_NAME,
                      e);
            // TODO: log here
            1
        }
        Err(PamInitError) => {
            eprintln!("{}: PAM initialization failure, aborting",
                      *PROG_NAME);
            // TODO: log here
            99
        },
        Err(PamError(e)) => {
            eprintln!("{}: PAM error: {}",
                      *PROG_NAME,
                      e);
            // TODO: log here
            1
        },
        Err(PamAbort) => {
            eprintln!("{}: abort requested by PAM", *PROG_NAME);
            // TODO: log here
            99
        },
        Err(TooManyLogins(n, host, username)) => {
            eprintln!("Maximum number of tries exceeded ({})", n);
            // TODO: log here
            let _h = host;
            let _u = username;
            0
        },
        Err(AuthTokenExpired) => {
            // Should be handled with a PAM chauthtok call
            eprintln!("{}: Password/authentication token expired",
                      *PROG_NAME);
            0
        },
        Err(CannotFindUser(u)) => {
            eprintln!("{}: Cannot find user ({})",
                      *PROG_NAME,
                      u);
            // TODO: log here
            1
        },
        Err(BadGroupID(i, u)) => {
            // TODO: log here
            let (_i, _u) = (i, u);
            1
        },
        Err(InitgroupsFailed(u)) => {
            // TODO: log here
            let _u = u;
            1
        },
        Err(TtyOwnership(d, u)) => {
            eprintln!("{}: Unable to change owner or mode of tty stdin: {}",
                      *PROG_NAME,
                      d);
            // TODO: log here
            let _u = u;
            1
        },
        Err(ShellError(e)) => {
            eprintln!("{}: Error spawning shell: {}",
                      *PROG_NAME,
                      e);
            // TODO: log here
            1
        }
    });
}
