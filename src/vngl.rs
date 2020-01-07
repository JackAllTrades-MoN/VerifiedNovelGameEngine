use std::io::BufReader;
use std::fs::File;
use crate::xml;
use crate::xml::reader::{EventReader, XmlEvent};
use crate::verror::{OrError};

#[derive(Debug)]
pub struct TagImg {
    pub src: String,
    pub x: u32,
    pub y: u32,
}

impl TagImg {
    pub fn new(attributes: Vec<xml::attribute::OwnedAttribute>) -> TagImg {
        let mut it = attributes.into_iter();
        let v = it.find(|attr| attr.name.local_name == "src").unwrap();
        let x = it.find(|attr| attr.name.local_name == "x")
            .map(|x| x.value.parse().unwrap()).unwrap_or(0);
        let y = it.find(|attr| attr.name.local_name == "y")
            .map(|y| y.value.parse().unwrap()).unwrap_or(0);
        TagImg{src:v.value, x:x, y:y}
    }
}

#[derive(Debug)]
pub struct TagSel {
    pub name: String,
    pub x: u32,
    pub y: u32,
}

impl TagSel {
    pub fn new(attributes: Vec<xml::attribute::OwnedAttribute>) -> TagSel {
        let mut it = attributes.into_iter();
        let name = it.find(|attr| attr.name.local_name == "name").unwrap();
        let x = it.find(|attr| attr.name.local_name == "x").unwrap();
        let y = it.find(|attr| attr.name.local_name == "y").unwrap();
        TagSel{name:name.value, x: x.value.parse().unwrap(), y: y.value.parse().unwrap()}
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
                        "img" => buff.push(InBody::Img(TagImg::new(attributes))),
                        "selection" => buff.push(InBody::Selection(TagSel::new(attributes))),
                        _ => ()
                    }
                },
                Ok(XmlEvent::EndElement { name }) => {
                    println!("{}", name);
                    match name.local_name.as_ref() {
                        "body" => (),
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
