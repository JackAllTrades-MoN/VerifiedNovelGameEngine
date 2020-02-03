
use sdl2::render::{Texture, Canvas};
use sdl2::rect::Rect;
use sdl2::video::Window;
use crate::vm::dom::{DOMTree, DOMType};

pub struct Drawable<'a> {
    texture: Texture<'a>,
    src: Option<Rect>,
    dst: Option<Rect>,
}

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
}

impl<'a> Drawable<'a> {
    pub fn draw(&self, canvas: &mut Canvas::<Window>) -> () {
        canvas.copy(&self.texture, self.src, self.dst).unwrap();
    }
}
