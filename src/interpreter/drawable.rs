
use sdl2::render::Texture;
use sdl2::rec::Rect;

use super::dom::{DOMNode, DOMType};

pub struct Drawable {
    texture: Texture,
    src: Option<Rect>,
    dst: Option<Rect>,
}

impl Drawable {
    pub fn from_dom(dom: &DOMNode) -> Option<Self> {
        match dom.dom_type {
            DOMType::Txt => {
                let ttf_context = sdl2::ttf::init().unwrap();
                let mut font = ttf_context.load_font(font_path, 128).unwrap();
                font.set_style(sdl2::ttf::FontStyle::BOLD);
                let surface = font.render("Test")
                    .blended(Color::RGBA(255,0, 0, 255)).unwrap();
                let texture =
                    texture_creator.create_texture_from_surface(&surface).unwrap();
                Some(Drawable {texture, None, None})
            },
            _ => None
        }
    }
}
