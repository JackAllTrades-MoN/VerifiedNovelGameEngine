use std::io::BufReader;
use std::fs::File;
use crate::xml;
use crate::xml::reader::{EventReader, XmlEvent};
use crate::verror::{OrError, VError, MayNecessary};

#[derive(Debug)]
pub struct TagImg {
    pub src: String,
    pub x: Option<u32>,
    pub y: Option<u32>,
    pub w: Option<u32>,
    pub h: Option<u32>,
}

impl TagImg {
    pub fn new(attributes: Vec<xml::attribute::OwnedAttribute>) -> OrError<TagImg> {
        let mut it = attributes.into_iter();
        let v = it.find(|attr| attr.name.local_name == "src").required("img", "src")?;
        let x = it.find(|attr| attr.name.local_name == "x").map(|x| x.value.parse().unwrap());
        let y = it.find(|attr| attr.name.local_name == "y").map(|y| y.value.parse().unwrap());
        let w = it.find(|attr| attr.name.local_name == "w").map(|w| w.value.parse().unwrap());
        let h = it.find(|attr| attr.name.local_name == "h").map(|h| h.value.parse().unwrap());
        Ok(TagImg{src:v.value, x:x, y:y, w:w, h:h})
    }
}

#[derive(Debug)]
pub struct TagSelItem {
    pub name: String,
    pub label: String,
}

impl TagSelItem {
    pub fn new(attributes: Vec<xml::attribute::OwnedAttribute>) -> OrError<TagSelItem> {
        let mut it = attributes.into_iter();
        let name = it.find(|attr| attr.name.local_name == "name")
            .required("item", "name")?;
        let label = it.find(|attr| attr.name.local_name == "label")
            .required("item", "label")?;
        Ok(TagSelItem { name: name.value, label: label.value })
    }
}

#[derive(Debug)]
pub struct TagSel {
    pub name: String,
    pub x: Option<u32>,
    pub y: Option<u32>,
    pub src: String,
    pub items: Vec<TagSelItem>,
}

/*
fn lookup_attr() -> OrError<> {
    let mut it 
}*/

impl TagSel {
    pub fn new(attributes: Vec<xml::attribute::OwnedAttribute>) -> OrError<TagSel> {
        let mut it = attributes.into_iter();
        let name = it.find(|attr| attr.name.local_name == "name")
            .required("selection", "name")?;
        let src = it.find(|attr| attr.name.local_name == "src")
            .required("selection", "src")?;
        let x = it.find(|attr| attr.name.local_name == "x");
        let y = it.find(|attr| attr.name.local_name == "y");
        Ok(TagSel{name:name.value,
                  x: x.map(|x| x.value.parse().unwrap()),
                  y: y.map(|y| y.value.parse().unwrap()),
                  src:src.value,
                  items:Vec::new()})
    }
}

#[derive(Debug)]
pub enum InBody {
    Img(TagImg),
    Selection(TagSel),
}

#[derive(Debug)]
pub struct Vngl {
    pub filename: String,
    pub body: Vec<InBody>,
}

impl Vngl {
    pub fn new(filename: &str) -> Vngl {
        Vngl { filename: filename.to_string(), body: Vec::new()}
    }

    pub fn from_file(filename: &str) -> OrError<Vngl> {
        let file = File::open(filename).unwrap();
        let file = BufReader::new(file);

        let mut vngl = Vngl::new(filename);
        let mut buff = Vec::new();

        let parser = EventReader::new(file);
        for e in parser {
            match e {
                Ok(XmlEvent::StartElement { name, attributes, .. }) => {
                    println!("{}", name);
                    match name.local_name.as_ref() {
                        "img" => {
                            let tagimg = TagImg::new(attributes)?;
                            buff.push(InBody::Img(tagimg))
                        },
                        "selection" => {
                            let tagsel = TagSel::new(attributes)?;
                            buff.push(InBody::Selection(tagsel))
                        },
                        "item" => {
                            let tagselitem = TagSelItem::new(attributes)?;
                            let idx = buff.len() - 1;
                            match &mut buff[idx] {
                                InBody::Selection(tagsel) => tagsel.items.push(tagselitem),
                                _ => assert!(false),
                            }
                        },
                        _ => ()
                    }
                },
                Ok(XmlEvent::EndElement { name }) => {
                    println!("{}", name);
                    match name.local_name.as_ref() {
                        "body" => (),
                        "item" => (),
                        _ => vngl.body.push(buff.pop().unwrap())
                    }
                },
                Err(e) => {
                    panic!("err")
                },
                _ => { println!("other") }
            }
        }
        println!("vngl: {:?}", vngl);
        Ok(vngl)
    }
}
