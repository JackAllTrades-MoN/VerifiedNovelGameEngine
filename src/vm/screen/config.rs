
use std::path::{Path, PathBuf};
use serde_derive::Deserialize;

#[derive(Debug, Deserialize)]
pub struct Config {
    pub title: String,
    pub window_w: u32,
    pub window_h: u32,
    pub fonts: Vec<(String, PathBuf)>
}

impl Config {
    pub fn default() -> Config {
        let title = "dummy".to_string();
        let (window_w, window_h) = (800, 600);
        let path = Path::new("./font/mplus-1p-regular.ttf").canonicalize().unwrap();
        let pathbuf = path.to_path_buf();
        let fonts = vec![("mplus".to_string(), pathbuf)];
        Config { title, window_w, window_h, fonts }
    }
}
