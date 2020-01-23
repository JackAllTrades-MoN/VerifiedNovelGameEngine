/// # Instruction 


// Primitives
pub enum Instruction {
    // DOM control
    UpdateDOMAttr,
    // var control
    UpdateGVar,
    // continuation control
    Jmp,
    SectionJmp,
    Quit,
}

/*
pub enum Instruction {
    UpdateText(String),
    LoadLayout(String),
}
*/
