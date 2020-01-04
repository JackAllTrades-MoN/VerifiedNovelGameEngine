//extern crate ini;
use crate::ini::Ini;
use crate::verror::{VError, OrError};

static DUMMYFILE: &str = "dummy";

pub struct Config {
    pub window_w: u16,
    pub window_h: u16,
    pub start_point: String
}

impl Config {
    pub fn default() -> Config {
        Config { window_w: 800, window_h: 600, start_point: DUMMYFILE.to_string()}
    }
    pub fn update_window_w(self, window_w: u16) -> Config {
        Config { window_w: window_w, .. self }
    }
    pub fn update_window_h(self, window_h: u16) -> Config {
        Config { window_h: window_h, .. self }
    }
    pub fn parse_from_file(filename: &str) -> OrError<Config> {
        Ini::load_from_file(filename)
            .and_then(|conf| {
                let section = conf.section(Some("Common".to_owned())).unwrap();
                let window_w = section.get("window_w").unwrap();
                let window_h = section.get("window_h").unwrap();
                Ok(Config::default()
                   .update_window_w(window_w.parse().unwrap())
                   .update_window_h(window_h.parse().unwrap()))
            })
//            .map(|conf| Config::default())
            .map_err(|err| VError::Other(err.to_string()))
    }
}
