//use std::cell::RefCell;

use sdl2::event::Event;
use sdl2::pixels::Color;
use sdl2::keyboard::Keycode;

use crate::verror::{OrError, VError};
use crate::project::{Project, Scene, Layout};


#[derive(Debug)]
pub enum Value {
    VInt(i64),
    VString(String),
}

#[derive(Debug)]
pub struct Interpreter <'a>{
    pub game: &'a Project,
    pub cur_scene: &'a Scene,
    pub cur_layout: &'a Layout,
    pub screen_txt: String,
    pub vars: Vec<(String, Value)>,
}

impl<'a> Interpreter<'a> {
    pub fn new(project: &'a Project) -> OrError<Interpreter<'a>> {
        let cfg = &project.config;
        println!("Project: {:?}", project);
        let cur_scene = project.scene_lookup(&cfg.initial_scene)
            .ok_or(VError::Other("initial_scene not found".to_string()))?;
        let cur_layout = project.layout_lookup(&cfg.initial_layout)
            .ok_or(VError::Other("initial_layout not found".to_string()))?;
        Ok(Interpreter {game: project,
                        cur_scene: cur_scene,
                        cur_layout: cur_layout,
                        screen_txt: "".to_string(),
                        vars: Vec::new()})
    }
    pub fn run(self) -> OrError<()>{
        let cfg = &self.game.config;
        let (title, window_w, window_h) = (&cfg.title, cfg.window_w, cfg.window_h);
        let sdl_context = sdl2::init().unwrap();
        let video_subsystem = sdl_context.video().unwrap();
        let window = video_subsystem.window(title, window_w, window_h)
            .position_centered()
            .build()
            .unwrap();
        let mut canvas = window.into_canvas().build().unwrap();
        let mut ip = self;
        canvas.set_draw_color(Color::RGB(0, 255, 255));
        canvas.clear();
        canvas.present();
        let mut event_pump = sdl_context.event_pump().unwrap();
        'running: loop {
            canvas.clear();
            for event in event_pump.poll_iter() {
                match event {
                    Event::Quit {..} |
                    Event::KeyDown { keycode: Some(Keycode::Escape), .. } => {
                        break 'running
                    },
                    _ => {}
                }
            }
            canvas.present();
        }
        Ok(())
    }
}
