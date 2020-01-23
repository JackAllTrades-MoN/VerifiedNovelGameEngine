
use crate::verror::{OrError, VError};
use super::instr::Instruction;

pub struct Script {
    name: String,
    body: Vec<Instruction>,
}

impl Script {
    pub fn from_file(name: &str) -> OrError<Script> {
        Err(VError::Unimplemented("Script::from_file"))
    }
}
