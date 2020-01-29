/// # Instruction 


// Primitives
pub enum Instruction {
    // DOM control
    UpdateDOMAttr(String, String, Value), //DOM id, attribute name, update value
    // var control
    UpdateGVar,
    // continuation control
    Jmp,
    SectionJmp(String), //section name
    Quit,
}

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

/*
pub enum Instruction {
    UpdateText(String),
    LoadLayout(String),
}
*/
