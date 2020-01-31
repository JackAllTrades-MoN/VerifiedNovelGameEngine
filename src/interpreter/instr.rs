/// # Instruction 
use super::dom::{DOMTree, DOMValue};

// Primitives
#[derive(Debug)]
pub enum Instruction {
    // DOM control
    UpdateDOMAttr(String, String, Value), //DOM id, attribute name, update value
    PutChild(String, DOMTree),
    // var control
    UpdateGVar,
    // continuation control
    Jmp,
    SectionJmp(String), //section name
    Quit,
}

#[derive(Debug)]
pub enum Value {
    VInt(i64),
    VString(String),
}

impl Instruction {
    // high-level instructions
    pub fn update_dialog(txt: &str) -> Instruction {
        Instruction::UpdateDOMAttr("dialog".to_string(),
                                   "value".to_string(),
                                   Value::VString(txt.to_string()))
    }
}

impl Value {
    pub fn to_dom_value (&self) -> DOMValue {
        match self {
            Value::VInt(i) => DOMValue::DInt(*i),
            Value::VString(s) => DOMValue::DString(s.to_string()),
        }
    }
}

/*
pub enum Instruction {
    UpdateText(String),
    LoadLayout(String),
}
*/
