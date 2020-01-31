
use serde_derive::Deserialize;

// Dummy
type GVar = i64

#[derive(Debug, Deserialize)]
pub struct Config {
    global_vars: Vec<GVar>,
    screen: super::screen::Config,
}

impl Config {
    pub fn default() -> Config {
        let global_vars = Vec::new();
        let screen = super::screen::Config::default();
        Config { global_vars, default }
    }
}
