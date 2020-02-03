

use std::path::{Path, PathBuf};
//use std::fs::{File};

//use ini::Ini;
use toml;
use serde_derive::Deserialize;

use crate::verror::{OrError};
//use crate::interpreter;
//use crate::interpreter::script::Script;
use crate::vm;
use crate::vm::script::Script;
use crate::compiler;

//project configuration
#[derive(Debug, Deserialize)]
pub struct Config {
    pub project_root: PathBuf,
    pub vm_config: vm::Config,
}

#[derive(Debug)]
pub enum Component {
    Scene{name: String, body: PathBuf},
}

#[derive(Debug)]
pub struct Project {
    pub config: Config,
    pub components: Vec<Component>,
}

pub fn dir_of (filename: &str) -> OrError<PathBuf> {
    let path = Path::new(filename);
    let fullpath = path.canonicalize()?;
    let parent = fullpath.parent().unwrap().to_path_buf();
    Ok(parent)
}

pub fn load_files<T>(dir: &Path, ext: &str, f: fn(&Path) -> T) -> OrError<Vec<T>> {
    let mut buff = Vec::<T>::new();
    for entry in std::fs::read_dir(dir)? {
        let entry = entry?;
        let path = entry.path();
        if let Some(extt) = path.extension() {
            if(extt == ext) { buff.push(f(&path)) }
        }
    }
    Ok(buff)
}
/*
impl Config {
    pub fn from_file(project_file: &str) -> OrError<Config> {
        let conf = Ini::load_from_file(project_file)?;
        let prj_root = dir_of(project_file)?;
        let interp_cfg = vm::Config::from_toml(project_file, conf)?;
        let cfg = Config { project_root: prj_root,
                           interp_cfg: interp_cfg };
        Ok(cfg)
    }
} */

impl Component {
    pub fn load_script(&self) -> OrError<Script> {
        match self {
            Component::Scene{name, body} => {
                let contents = std::fs::read_to_string(body)?;
                let body = compiler::parse(&contents)?;
                Ok(Script {name: name.to_string(),
                           body: body})
            },
        }
    }
}

impl Project {
    pub fn load_project(project_file: &str) -> OrError<Project> {
        let s = std::fs::read_to_string(project_file)?;
        let cfg : Config = toml::from_str(&s)?;
//        let cfg = Config::from_file(project_file)?;
        let project_dir = dir_of(project_file)?;
        let dir = std::fs::read_dir(project_dir)?;
        let mut components = Vec::new();
        for entry in dir {
            let entry = entry?;
            let path = entry.path();
            if path.is_dir() {
                let dirname = path.file_name().unwrap().to_str().unwrap();
                match &*dirname {
                    "scene" => {
                        let f = |sp: &Path| {
                            let name = sp.file_name().unwrap().to_str().unwrap().to_string();
                            let body = sp.to_path_buf();
                            Component::Scene{name: name, body: body}
                        };
                        components = load_files(&path, "scene", f)?;
                    },
                    _ => (),
                }
            }
        };
        let project = Project { config: cfg, components: components };
        Ok(project)
    }
}
