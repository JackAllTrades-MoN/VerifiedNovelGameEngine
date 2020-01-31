use sdl2::{Sdl, EventPump};
use sdl2::render::Canvas;
use sdl2::video::Window;
use sdl2::ttf::Font;
use sdl2::pixels::Color;

use crate::verror::OrError;

pub struct Screen<'a, 'b> {
    sdl_context: Sdl,
    canvas: Canvas::<Window>,
    epump: EventPump,
    fonts: Vec<Font<'a, 'b>>
}

impl<'a, 'b> Screen<'a, 'b> {
    pub fn new(title: &str,
               window_w: u32,
               window_h: u32,) -> OrError<Screen>{
        let sdl_context = sdl2::init().unwrap();
        let ttf_context = sdl2::ttf::init().unwrap();
        let video_subsystem = sdl_context.video().unwrap();
        let window = video_subsystem.window(title, window_w, window_h)
            .position_centered()
            .build()
            .unwrap();
        let canvas = window.into_canvas().build().unwrap();
        let epump = sdl_context.event_pump().unwrap();
        let fonts = Vec::new();
        Ok(Screen{sdl_context, canvas, epump, fonts})
    }
    pub fn update(&mut self) -> () {
        self.canvas.set_draw_color(Color::RGB(0, 255, 255));
        self.canvas.clear();
        self.canvas.present();
    }
}
