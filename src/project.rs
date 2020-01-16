
use crate::ini::Ini;
use crate::verror::{VError, OrError, MayNecessary};

use std::path::{Path, PathBuf};

static DUMMYFILE: &str = "dummy";
//static DUMMYPATH: &Path = Path::new("dummy");
static DUMMYTITLE: &str = "dummy";

/*
pub fn fullpath_of(filename: &str) -> OrError<std::path::PathBuf> {
    // TODO: This is just stub implementation
    let path = Path::new(filename);
    let fullpath = path.canonicalize()?;
    let parent = fullpath.parent().unwrap();
    Ok(parent)
}*/

pub fn dir_of (filename: &str) -> OrError<std::path::PathBuf> {
    let path = Path::new(filename);
    let fullpath = path.canonicalize()?;
    let parent = fullpath.parent().unwrap().to_path_buf();
    Ok(parent)
}

#[derive(Debug)]
pub struct Config {
    pub project_root: PathBuf,
    pub window_w: u32,
    pub window_h: u32,
    pub title: String,
    pub initial_scene: String,
    pub params: Vec<(String, String)>,
    pub fonts: Vec<(String, String)>
}

#[derive(Debug)]
pub enum RawOrName<T> {
    Raw(T), // for embedded resources (in production environment)
    Name(PathBuf),
}

#[derive(Debug)]
pub struct Scene {
    pub name: String,
    pub body: RawOrName<String>,
}

#[derive(Debug)]
pub struct Project {
    pub config: Config,
    pub layout: Vec<RawOrName<String>>,
    pub scene: Vec<Scene>,
}

impl Config {
    pub fn from_file(filename: &str) -> OrError<Config>{
        let conf = Ini::load_from_file(filename)?;
        let common = conf.section(Some("Common".to_string()))
            .csrequired(filename, "Common")?;
        let window_w = common.get("window_w")
            .carequired(filename, "Common", "window_w")?;
        let window_h = common.get("window_h")
            .carequired(filename, "Common", "window_h")?;
        let title = common.get("title")
            .carequired(filename, "Common", "title")?;
        let initial_scene = common.get("initial_scene")
            .carequired(filename, "Common", "initial_scene")?;
//        let project_root = Path::new(filename).canonicalize()?;
        let project_root = dir_of(filename)?;
        let cfg = Config {
            project_root: project_root,
            window_w: window_w.parse().unwrap(),
            window_h: window_h.parse().unwrap(),
            title: title.to_string(),
            initial_scene: initial_scene.to_string(),
            params: Vec::new(),
            fonts: Vec::new()
        };
        Ok(cfg)
    }
}

impl Project {
    pub fn load_project(project_file: &str) -> OrError<Project> {
//        let project_root = Path::new(project_file).canonicalize()?;
//        let project_dir = project_root.parent().unwrap();
        let project_dir = dir_of(project_file)?;
        let config = Config::from_file(project_file)?;
        let dir = std::fs::read_dir(project_dir)?;
        let mut scene = Vec::new();
        let mut layout = Vec::new();
        for entry in dir {
            let entry = entry?;
            let path = entry.path();
            if let Some(ext) = path.extension() {
                if(ext == "scene") {
//                    println!("scene file: {}", path.to_str().unwrap())
                    scene.push(Scene{
                        name: path.file_name().unwrap().to_str().unwrap().to_string(),
                        body: RawOrName::Name(path.to_path_buf()),
                    });
                } else if(ext == ".vngl") {
                    layout.push(RawOrName::Name(path.to_path_buf()))
                }
            }
        }
        let project = Project { config: config, scene: scene, layout:layout };
        Ok(project)
    }
    pub fn scene_lookup(&self, scene_name: &str) -> Option<&Scene> {
        let mut it = self.scene.iter();
        it.find(|scene| scene.name == scene_name)
    }
}
