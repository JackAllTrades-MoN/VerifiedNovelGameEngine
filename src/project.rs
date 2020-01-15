
use crate::ini::Ini;
use crate::verror::{VError, OrError};

use std::path::Path;

static DUMMYFILE: &str = "dummy";
static DUMMYPATH: &str = "dummy";
static DUMMYTITLE: &str = "dummy";

pub fn dir_of(filename: &str) -> String {
    // TODO: This is just stub implementation
    let path = Path::new(filename);
    let fullpath = path.canonicalize().unwrap();
    let parent = fullpath.parent().unwrap();
    let sp = parent.to_str().unwrap();
    sp.to_string()
}

pub struct Config {
    pub project_root: String,
    pub window_w: f64,
    pub window_h: f64,
    pub title: String,
    pub initial_scene: String,
    pub params: Vec<(String, String)>,
    pub fonts: Vec<(String, String)>
}

pub enum RawOrName<T> {
    Raw(T), // for embedded resources (in production environment)
    Name(String),
}

pub struct Scene {
    pub name: String,
    pub body: RawOrName<String>,
    pub layout: RawOrName<String>,
}

pub struct Project {
    pub config: Config,
    pub scene: Vec<Scene>,
}

impl Config {
    pub fn default() -> Config {
        Config { project_root: DUMMYPATH.to_string(),
                 window_w: 800.0, window_h: 600.0,
                 title: DUMMYTITLE.to_string(),
                 initial_scene: DUMMYPATH.to_string(),
                 params: Vec::new(),
                 fonts: Vec::new()
        }
    }

    pub fn update_window_w(self, window_w: &str) -> Config {
        Config { window_w: window_w.parse().unwrap(), .. self }
    }

    pub fn update_window_h(self, window_h: &str) -> Config {
        Config { window_h: window_h.parse().unwrap(), .. self }
    }

    pub fn update_project_root(self, project_root: &str) -> Config {
        Config { project_root: project_root.to_string(), .. self }
    }

    pub fn update_title(self, title: &str) -> Config {
        Config { title: title.to_string(), .. self }
    }

    pub fn from_file(filename: &str) -> OrError<Config>{
        Ini::load_from_file(filename)
            .and_then(|conf| {
                let common = conf.section(Some("Common".to_owned())).unwrap();
                let window_w = common.get("window_w").unwrap();
                let window_h = common.get("window_h").unwrap();
                let title = common.get("title").unwrap();
                let initial_scene = common.get("title.scene").unwrap();
                let project_root = dir_of(filename);
                let cfg = Config::default()
                    .update_window_w(window_w)
                    .update_window_h(window_h)
                    .update_title(title)
                    .update_project_root(&project_root);
                Ok(cfg)
            })
            .map_err(|err| VError::Other(err.to_string()))
    }
}

impl Project {
    pub fn load_project(project_file: &str) -> OrError<Project> {
        let _project_dir = dir_of(project_file);
        let config = Config::from_file(project_file)?;
        let project = Project { config: config, scene: Vec::new() };
        Ok(project)
    }
}
