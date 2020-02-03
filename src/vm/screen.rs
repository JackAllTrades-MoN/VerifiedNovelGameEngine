pub mod config;
pub mod drawable;

use sdl2::{Sdl, EventPump};
use sdl2::render::Canvas;
use sdl2::video::Window;
use sdl2::ttf::Font;
use sdl2::pixels::Color;

use crate::verror::OrError;
use drawable::Drawable;
use super::dom::DOMTree;

pub type Config = config::Config;

pub struct Screen<'a, 'b> {
    pub sdl_context: Sdl,
    pub canvas: Canvas::<Window>,
    pub epump: EventPump,
    pub fonts: Vec<Font<'a, 'b>>,
    pub drawable: Vec<Drawable<'a>>,
}

impl<'a, 'b> Screen<'a, 'b> {
    pub fn new(cfg: &Config) -> OrError<Screen> {
        let sdl_context = sdl2::init().unwrap();
        let ttf_context = sdl2::ttf::init().unwrap();
        let video_subsystem = sdl_context.video().unwrap();
        let window = video_subsystem.window(&cfg.title, cfg.window_w, cfg.window_h)
            .position_centered()
            .build()
            .unwrap();
        let canvas = window.into_canvas().build().unwrap();
        let epump = sdl_context.event_pump().unwrap();
        let fonts = Vec::new();
        let drawable = Vec::new();
        Ok(Screen{sdl_context, canvas, epump, fonts, drawable})
    }
    pub fn update(&mut self) -> () {
        self.canvas.set_draw_color(Color::RGB(0, 255, 255));
        self.canvas.clear();
        let it = self.drawable.iter();
        for d in it {
            d.draw(&mut self.canvas);
        }
        self.canvas.present();
    }
    pub fn update_drawable(&mut self, dom: &'a DOMTree) -> () {
        let new_drawable = drawable::from_dom(dom);
        self.drawable = new_drawable;
    }
}
