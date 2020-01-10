//extern crate ini;
use crate::ini::Ini;
use crate::verror::{VError, OrError};

use std::path::Path;

static DUMMYFILE: &str = "dummy";
static DUMMYPATH: &str = "dummy";

pub struct Config {
    pub project_root: String,
    pub window_w: f64,
    pub window_h: f64,
    pub start_point: String
}

pub fn dir_of(filename: &str) -> String {
    // TODO: This is just stub implementation
    let path = Path::new(filename);
    let fullpath = path.canonicalize().unwrap();
    let parent = fullpath.parent().unwrap();
    let sp = parent.to_str().unwrap();
    println!("parent: {}", sp);
    sp.to_string()
}

impl Config {
    pub fn default() -> Config {
        Config { project_root: DUMMYPATH.to_string(),
                 window_w: 800.0, window_h: 600.0,
                 start_point: DUMMYFILE.to_string()}
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

    pub fn parse_from_file(filename: &str) -> OrError<Config> {
        Ini::load_from_file(filename)
            .and_then(|conf| {
                let section = conf.section(Some("Common".to_owned())).unwrap();
                let window_w = section.get("window_w").unwrap();
                let window_h = section.get("window_h").unwrap();
                let project_root = section.get("project_root");
                let cfg = Config::default()
                    .update_window_w(window_w)
                    .update_window_h(window_h);
                match project_root {
                    None => Ok(cfg.update_project_root(&dir_of(filename))),
                    Some(pr) => Ok(cfg.update_project_root(pr)),
                }
            })
//            .map(|conf| Config::default())
            .map_err(|err| VError::Other(err.to_string()))
    }
}
