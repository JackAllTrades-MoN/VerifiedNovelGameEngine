use crate::verror::{OrError};
use crate::project::{Project, Scene};


#[derive(Debug)]
pub enum Value {
    VInt(i64),
    VString(String),
}

#[derive(Debug)]
pub struct Interpreter {
    pub game: Project,
    pub cur_scene: Scene,
    pub vars: Vec<(String, Value)>,
}

impl Interpreter {
    pub fn run(project: &Project) -> OrError<()>{
        Ok(())
    }
}
