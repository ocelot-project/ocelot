use ::pam_sys;
use ::pam_sys::{PamReturnCode, PamItemType};
pub use ::pam_sys::{PamMessage, PamMessageStyle, PamResponse};
use ::{OlmanError, PROG_NAME, AUTH_STORE};
use std::ffi::{CString, CStr};
use std::collections::HashMap;

#[allow(dead_code)]
pub enum PamStringItem {
    ServiceName,
    User,
    UserPrompt,
    TTY,
    RequestingUser,
    RequestingHost,
    AuthToken,
    OldAuthToken,
    XDisplay,
    AuthTokenType
}

pub enum AuthStatus {
    Authenticated,
    MaxTriesExceeded,
    Abort,
    Failed(String)
}

extern "C" {
    pub fn pam_fail_delay(handle: *mut pam_sys::PamHandle,
                          usec: ::libc::c_uint)
                          -> ::libc::c_int;
}

impl PamStringItem {
    fn to_pam_item(&self) -> PamItemType {
        match *self {
            PamStringItem::ServiceName => PamItemType::SERVICE,
            PamStringItem::User => PamItemType::USER,
            PamStringItem::UserPrompt => PamItemType::USER_PROMPT,
            PamStringItem::TTY => PamItemType::TTY,
            PamStringItem::RequestingUser => PamItemType::RUSER,
            PamStringItem::RequestingHost => PamItemType::RHOST,
            PamStringItem::AuthToken => PamItemType::AUTHTOK,
            PamStringItem::OldAuthToken => PamItemType::OLDAUTHTOK,
            PamStringItem::XDisplay => PamItemType::XDISPLAY,
            PamStringItem::AuthTokenType => PamItemType::AUTHTOK_TYPE

        }
    }
}

pub struct PamSession<'a> {
    last_return: pam_sys::PamReturnCode,
    has_open_session: bool,
    handle: &'a mut pam_sys::PamHandle
}

impl<'a> PamSession<'a> {
    pub fn new<S>(service: S,
                  conversation: Option<pam_sys::types::ConvClosure>)
        -> Result<Self, OlmanError>
    where S: Into<String> {
        let mut handle: *mut pam_sys::PamHandle = ::std::ptr::null_mut();

        match pam_sys::start(&service.into(),
                             None,
                             &pam_sys::PamConversation {
                                 conv: conversation,
                                 data_ptr: ::std::ptr::null_mut()
                             },
                             &mut handle) {
            pam_sys::PamReturnCode::SUCCESS => unsafe {
                Ok(Self {
                    last_return: PamReturnCode::SUCCESS,
                    has_open_session: false,
                    handle: &mut *handle
                })
            },
            _ => Err(OlmanError::PamInitError)
        }
    }

    pub fn set_string_item<S>(&mut self, item_type: PamStringItem, value: S)
                              -> Result<(), OlmanError>
    where S: Into<String>  {
        let v = CString::new(value.into()).unwrap();
        let ret = unsafe {
            pam_sys::raw::pam_set_item(self.handle,
                                       item_type.to_pam_item() as ::libc::c_int,
                                       v.as_ptr() as *const ::libc::c_void)
        }.into();
        self.last_return = ret;
        match ret {
            PamReturnCode::SUCCESS => Ok(()),
            e => Err(OlmanError::PamError(
                pam_sys::strerror(self.handle, e).unwrap().into()))
        }
    }

    pub fn get_string_item(&mut self, item_type: PamStringItem)
                           -> Result<String, OlmanError> {
        let value_ptr = ::std::ptr::null_mut()
            as *const ::libc::c_char;
        let ret = pam_sys::get_item(self.handle,
                                    item_type.to_pam_item(),
                                    &mut (value_ptr
                                          as *const ::libc::c_void));

        match ret {
            PamReturnCode::SUCCESS => {
                if value_ptr.is_null() {
                    Err(OlmanError::PamError(
                        "get_string_item returned null".into()))
                }
                else {
                    let value_cstr = unsafe { CStr::from_ptr(value_ptr) };
                    Ok(String::from_utf8_lossy(value_cstr
                                               .to_owned()
                                               .as_bytes())
                       .into())
                }
            }
            e => Err(OlmanError::PamError(
                pam_sys::strerror(self.handle, e).unwrap().into()))
        }
    }

    pub fn set_fail_delay(&mut self, delay: usize) -> Result<(), OlmanError> {
        let ret = unsafe {
            pam_fail_delay(self.handle, delay as ::libc::c_uint)
        }.into();
        self.last_return = ret;

        match ret {
            PamReturnCode::SUCCESS => Ok(()),
            e => Err(OlmanError::PamError(
                pam_sys::strerror(self.handle, e).unwrap().into()))
        }
    }

    pub fn authenticate(&mut self) -> AuthStatus {
        let ret = pam_sys::authenticate(self.handle, pam_sys::PamFlag::NONE);
        self.last_return = ret;

        match ret {
            PamReturnCode::SUCCESS => AuthStatus::Authenticated,
            PamReturnCode::MAXTRIES => AuthStatus::MaxTriesExceeded,
            PamReturnCode::ABORT => AuthStatus::Abort,
            e => AuthStatus::Failed(
                pam_sys::strerror(self.handle, e).unwrap().into())
        }
    }

    pub fn validate_account(&mut self) -> Result<(), OlmanError> {
        let ret = pam_sys::acct_mgmt(self.handle, pam_sys::PamFlag::NONE);
        self.last_return = ret;

        match ret {
            PamReturnCode::SUCCESS => Ok(()),
            PamReturnCode::NEW_AUTHTOK_REQD =>
                Err(OlmanError::AuthTokenExpired),
            e => Err(OlmanError::PamError(
                pam_sys::strerror(self.handle, e).unwrap().into()))
        }
    }

    pub fn change_expired_token(&mut self) -> Result<(), OlmanError> {
        let ret = pam_sys::chauthtok(self.handle, pam_sys::PamFlag::NONE);
        self.last_return = ret;

        match ret {
            PamReturnCode::SUCCESS => Ok(()),
            e => Err(OlmanError::PamError(
                pam_sys::strerror(self.handle, e).unwrap().into()))
        }
    }

    pub fn open_session(&mut self, hushed: bool) -> Result<(), OlmanError> {
        let ret = if hushed {
            pam_sys::open_session(self.handle, pam_sys::PamFlag::SILENT)
        }
        else {
            pam_sys::open_session(self.handle, pam_sys::PamFlag::NONE)
        };
        self.last_return = ret;

        match ret {
            PamReturnCode::SUCCESS => {
                self.has_open_session = true;
                Ok(())
            },
            e => Err(OlmanError::PamError(
                pam_sys::strerror(self.handle, e).unwrap().into()))
        }
    }

    pub fn establish_credentials(&mut self) -> Result<(), OlmanError> {
        let ret = pam_sys::setcred(self.handle,
                                   pam_sys::PamFlag::ESTABLISH_CRED);
        self.last_return = ret;

        match ret {
            PamReturnCode::SUCCESS => Ok(()),
            e => Err(OlmanError::PamError(
                pam_sys::strerror(self.handle, e).unwrap().into()))
        }
    }

    pub fn get_env(&mut self) -> Option<PamEnvVars> {
        PamEnvVars::new(self.handle)
    }
}

impl<'a> Drop for PamSession<'a> {
    fn drop(&mut self) {
        if self.has_open_session {
            pam_sys::close_session(self.handle, pam_sys::PamFlag::NONE);
        }
        pam_sys::end(self.handle, self.last_return);
    }
}

// Based on pam-auth's implementation
pub struct PamEnvVars {
    ptr: *const *const ::libc::c_char
}

impl PamEnvVars {
    fn new(handle: &mut pam_sys::PamHandle) -> Option<Self> {
        let env = pam_sys::getenvlist(handle);

        if !env.is_null() {
            Some(Self {
                ptr: env
            })
        }
        else {
            None
        }
    }

    pub fn to_map(&mut self) -> HashMap<String, String> {
        let mut m = HashMap::new();
        let mut i = 0;
        loop {
            unsafe {
                let env_ptr: *const *const ::libc::c_char = self.ptr.offset(i);
                if !(*env_ptr).is_null() {
                    i += 1;

                    let env = CStr::from_ptr(*env_ptr).to_string_lossy();
                    let parts: Vec<_> = env.splitn(2, '=').collect();

                    if parts.len() == 2 {
                        m.insert(parts[0].into(), parts[1].into());
                    }
                }
                else {
                    break;
                }
            }
        }

        m
    }
}

pub extern "C" fn pam_conversation(num_msg: ::libc::c_int,
                                   msg: *mut *mut PamMessage,
                                   resp: *mut *mut PamResponse,
                                   _appdata: *mut ::libc::c_void)
                                   -> ::libc::c_int {
    // TODO: I hate this
    unsafe {
        *resp = ::libc::calloc(num_msg as usize,
                               ::std::mem::size_of::<PamResponse>()
                               as ::libc::size_t)
            as *mut PamResponse;

        if(*resp).is_null() {
            return PamReturnCode::BUF_ERR as ::libc::c_int;
        }

        let mut result: PamReturnCode = PamReturnCode::SUCCESS;

        for i in 0..num_msg as isize {
            let m: &mut PamMessage = &mut **(msg.offset(i));
            let r: &mut PamResponse = &mut *((*resp).offset(i));

            match PamMessageStyle::from(m.msg_style) {
                PamMessageStyle::PROMPT_ECHO_ON => {
                    eprintln!(
                        "{}: PAM modules requesting echoing are not supported",
                        *PROG_NAME);
                    result = pam_sys::PamReturnCode::CONV_ERR;
                },
                PamMessageStyle::PROMPT_ECHO_OFF => {
                    // Assume PAM is requesting a password
                    if let Ok(mut auth_store) = AUTH_STORE.lock() {
                        if auth_store.with_credentials(|_u, password, _s| {
                            if let Ok(pw) = CString::new(password) {
                                r.resp =
                                    ::libc::strdup(pw.as_ptr()
                                                      as *const ::libc::c_char);
                                Ok(())
                            }
                            else {
                                Err(OlmanError::PamAbort)
                            }
                        }).is_err() {
                            result = pam_sys::PamReturnCode::CONV_ERR;
                        }
                    }
                    else {
                        eprintln!("{}: auth lock poisoned",
                                  *PROG_NAME);
                        result = pam_sys::PamReturnCode::CONV_ERR;
                    }
                },
                PamMessageStyle::ERROR_MSG => {
                    result = PamReturnCode::CONV_ERR;
                },
                PamMessageStyle::TEXT_INFO => {
                    eprintln!("{}: (PAM) {}",
                              *PROG_NAME,
                              String::from_utf8_lossy(
                                  CStr::from_ptr(m.msg).to_bytes()));
                }
            }

            if result != PamReturnCode::SUCCESS {
                ::libc::free(*resp as *mut ::libc::c_void);
                break;
            }
        }

        result as ::libc::c_int
    }
}
