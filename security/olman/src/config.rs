use std::sync::Mutex;

#[derive(Deserialize, Debug)]
pub struct Config {
    #[serde(default)]
    pub olman: Olman,
    #[serde(default)]
    pub frontend: Frontend,
    #[serde(default)]
    pub graphical: Graphical,
    #[serde(default = "default_session_list")]
    pub session: Vec<Session>
}

fn default_session_list() -> Vec<Session> {
    vec![Session {
        name: "Default Shell".into(),
        graphical: false,
        command: None,
        shell: None
    }]
}

impl Default for Config {
    fn default() -> Self {
        Self {
            olman: Olman::default(),
            frontend: Frontend::default(),
            graphical: Graphical::default(),
            session: default_session_list()
        }
    }
}

#[derive(Deserialize, Debug)]
#[serde(default)]
pub struct Olman {
    pub session_default_shell: String,
    pub command_flag: String
}

impl Default for Olman {
    fn default() -> Self {
        Self {
            session_default_shell: "/usr/bin/bash".into(),
            command_flag: "-c".into()
        }
    }
}

#[derive(Deserialize, Debug)]
#[serde(default)]
pub struct Frontend {
    pub user: String,
    pub group: String
}

impl Default for Frontend {
    fn default() -> Self {
        Self {
            user: "nobody".into(),
            group: "nogroup".into()
        }
    }
}

#[derive(Deserialize, Debug)]
#[serde(default)]
pub struct Graphical {
    pub command: String
}

impl Default for Graphical {
    fn default() -> Self {
        Self {
            command: "startx".into()
        }
    }
}

#[derive(Clone, Deserialize, Debug)]
pub struct Session {
    #[serde(default = "default_session_name")]
    pub name: String,
    #[serde(default = "default_session_graphical")]
    pub graphical: bool,
    pub command: Option<String>,
    pub shell: Option<String>
}

fn default_session_name() -> String {
    lazy_static! {
        static ref SESSION_NUM: Mutex<u16> = Mutex::new(0);
    }
    let mut session_num = SESSION_NUM.lock().unwrap();

    *session_num += 1;
    format!("Session {}", session_num)
}

#[inline]
fn default_session_graphical() -> bool {
    false
}

impl Default for Session {
    fn default() -> Self {
        Self {
            name: default_session_name(),
            graphical: default_session_graphical(),
            command: None,
            shell: None
        }
    }
}
