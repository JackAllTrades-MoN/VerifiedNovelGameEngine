
use crate::verror::{VError, OrError};

pub type DOMTree = DOMNode;
pub type DOMAttribute = (String, DOMValue);

#[derive(Debug)]
pub enum DOMType { Root, General, Txt }

#[derive(Debug)]
pub struct DOMNode {
    dom_type: DOMType,
    id: String,
    name: String,
    children: Vec<DOMNode>,
    attributes: Vec<DOMAttribute>
}

#[derive(Debug)]
pub enum DOMValue {
    DInt(i64),
    DString(String),
}

pub fn root() -> DOMNode {
    DOMNode {
        dom_type: DOMType::Root,
        id:"root".to_string(),
        name:"root".to_string(),
        children:Vec::new(),
        attributes:Vec::new()}
}

impl DOMNode {
    pub fn lookup_by_id(&mut self, id: &str) -> Option<&mut DOMNode> {
        let mut it = self.children.iter_mut();
        it.find(|d| d.id == id)
    }
    pub fn update_attr(&mut self, attr_name: &str, value: DOMValue) -> OrError<()> {
        let mut it = self.attributes.iter();
        let target_idx: usize = it.position(|(aname, _)| aname == attr_name)
            .ok_or(VError::Other("Attribute Not Found".to_string()))?;
        self.attributes[target_idx] = (attr_name.to_string(), value);
        Ok(())
    }
}
