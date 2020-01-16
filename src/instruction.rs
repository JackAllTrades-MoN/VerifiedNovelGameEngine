use crate::project::Scene;
use crate::verror::{OrError, VError};

#[derive(Debug)]
pub enum Instruction {
    UpdateText(String),
    LoadLayout(String),
}


impl Instruction {
    fn line_decode(scene: &str) -> OrError<Instruction> {
        Result::Err(VError::Other("unimplemented".to_string()))
    }
}
