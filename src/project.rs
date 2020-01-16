
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
    pub initial_layout: String,
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
pub struct Layout {
    pub name: String,
    pub body: RawOrName<String>,
}

#[derive(Debug)]
pub struct Project {
    pub config: Config,
    pub layout: Vec<Layout>,
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
        let initial_layout = common.get("initial_layout")
            .carequired(filename, "Common", "initial_layout")?;
//        let project_root = Path::new(filename).canonicalize()?;
        let project_root = dir_of(filename)?;
        let cfg = Config {
            project_root: project_root,
            window_w: window_w.parse().unwrap(),
            window_h: window_h.parse().unwrap(),
            title: title.to_string(),
            initial_scene: initial_scene.to_string(),
            initial_layout: initial_layout.to_string(),
            params: Vec::new(),
            fonts: Vec::new()
        };
        Ok(cfg)
    }
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
            if path.is_dir() {
                let dirname = path.file_name().unwrap().to_str().unwrap();
                if(dirname == "layout") {
                    layout =
                        load_files(
                            &path,
                            "vngl",
                            |lp| Layout{
                                name:lp.file_name()
                                    .unwrap().to_str().unwrap().to_string(),
                                body:RawOrName::Name(lp.to_path_buf())})?;
                } else if(dirname == "scene") {
                    scene =
                        load_files(
                            &path,
                            "scene",
                            |sp| Scene {
                                name:sp.file_name()
                                    .unwrap().to_str().unwrap().to_string(),
                                body:RawOrName::Name(sp.to_path_buf())})?;
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
    pub fn layout_lookup(&self, layout_name: &str) -> Option<&Layout> {
        let mut it = self.layout.iter();
        it.find(|layout| layout.name == layout_name)
    }
}
