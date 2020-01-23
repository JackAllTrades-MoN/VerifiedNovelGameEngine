
// Dummy Impl
pub enum DOMTree {
    DOMNode,
}

// Dummy Impl
enum DOMType {
    Layout,
}

impl DOMTree {
    pub fn root() -> DOMTree {
        DOMTree::DOMNode
    }
}
