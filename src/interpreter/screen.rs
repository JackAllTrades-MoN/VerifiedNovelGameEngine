use std::path::PathBuf;

use ini::Ini;
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

pub struct Config {
    title: String,
    window_w: u32,
    window_h: u32,
    fonts: Vec<(String, PathBuf)>
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

impl Config {
    pub fn default() -> Config {
        Config {title: "dummy".to_string(), window_w: 800, window_h: 600 }
    }
    pub fn from_ini(filename: &str, ini: Ini) -> OrError<Config> {
        let screen_cfg = ini.section(Some("Screen".to_string()))
            .csrequired(filename, "Interpreter")?;
        let window_w = interp_cfg.get("window_w")
            .carequired(filename, "Interpreter", "window_w")?;
        let window_h = interp_cfg.get("window_h")
            .carequired(filename, "Interpreter", "window_h")?;
        let title = interp_cfg.get("title")
            .carequired(filename, "Interpreter", "title")?;
        Ok(Config { title: title.to_string(),
                    window_w: window_w.parse().unwrap(),
                    window_h: window_h.parse().unwrap() })
    }
}
