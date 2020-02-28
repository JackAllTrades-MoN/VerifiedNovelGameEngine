
use sdl2::render::{Texture, Canvas};
use sdl2::rect::Rect;
use sdl2::video::Window;
use sdl2::pixels::Color;
use sdl2::ttf::Font;
use crate::vm::dom::{DOMTree, DOMType};

pub enum Drawable<'a> {
    Dialog{text: String, font: Font<'a, 'static>},
    Img{texture: Texture<'a>, src: Option<Rect>, dst: Option<Rect>}
}

pub fn from_dom_aux<'a>(acc: Vec<Drawable<'a>>, dom: &DOMTree) -> Vec<Drawable<'a>> {
    match dom.dom_type {
        DOMType::Root => {
            dom.children.iter().fold(acc, |acc, cdom| from_dom_aux(acc, cdom))
        },
        DOMType::Dialog => {
            acc
        },
        _ => { acc }
    }
}

pub fn from_dom(dom: &DOMTree) -> Vec<Drawable> {
    let mut vec = Vec::new();
    from_dom_aux(vec, dom)
}

/*
pub fn from_dom(dom: &DOMTree) -> Vec<Drawable> {
    let mut drawable = Vec::new();
/*    match dom.dom_type {
        DOMType::Root => {
            let it = dom.children.iter();
            for 
        },
        DOMType::Txt => {
            let ttf_context = sdl2::ttf::init().unwrap();
            let mut font = ttf_context.load_font(font_path, 128).unwrap();
            font.set_style(sdl2::ttf::FontStyle::BOLD);
            let surface = font.render("Test")
                .blended(Color::RGBA(255,0, 0, 255)).unwrap();
            let texture =
                texture_creator.create_texture_from_surface(&surface).unwrap();
            Some(Drawable {texture, src:None, dst:None})
        },
        _ => None
    };*/
    drawable
}*/

impl<'a> Drawable<'a> {
    pub fn draw(&self, canvas: &mut Canvas::<Window>) -> () {
        //canvas.copy(&self.texture, self.src, self.dst).unwrap();
        match self {
            Drawable::Dialog{text: text, font: font} => {
//                let ttf_context = sdl2::ttf::init().unwrap();
                let texture_creator = canvas.texture_creator();
/*                let mut font = ttf_context.load_font("./font/mplus-1p-regular.ttf", 128)
                    .unwrap();*/
                let surface = font.render(text)
                    .blended(Color::RGBA(0, 0, 0, 255)).unwrap();
                let texture =
                    texture_creator.create_texture_from_surface(&surface).unwrap();
                canvas.copy(&texture, None, None).unwrap();
            },
            Drawable::Img{texture: texture, src: src, dst: dst} =>
            {
                canvas.copy(&texture, *src, *dst).unwrap();
            },
        }
    }
}
