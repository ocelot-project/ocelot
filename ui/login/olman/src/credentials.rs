pub struct Credentials {
    username: String,
    password: String,
    session: u16
}

impl Credentials {
    pub fn new() -> Self {
        Self {
            username: "".into(),
            password: "".into(),
            session: 0
        }
    }

    pub fn erase(&mut self) {
        self.username = "".into();
        self.password = "".into();
        self.session = 0;
    }

    pub fn with_credentials<F>(&mut self, closure: F) -> Result<(), ::OlmanError>
    where F: FnOnce(&str, &str, u16) -> Result<(), ::OlmanError> {
        let ret = closure(&self.username, &self.password, self.session);
        self.erase();
        ret
    }

    pub fn set_credentials<F>(&mut self, closure: F) -> Result<(), ::OlmanError>
    where F: FnOnce() -> Result<(String, String, u16), ::OlmanError> {
        match closure() {
            Ok((u, p, s)) => {
                self.username = u;
                self.password = p;
                self.session = s;
                Ok(())
            },
            Err(e) => {
                self.erase();
                Err(e)
            }
        }
    }
}

impl Drop for Credentials {
    fn drop(&mut self) {
        self.erase();
    }
}
