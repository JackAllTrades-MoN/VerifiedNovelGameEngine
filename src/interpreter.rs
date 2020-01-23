mod dom;
mod instr;
pub mod memory;

use sdl2::pixels::Color;

use instr::Instruction;
use dom::DOMTree;
use memory::Memory;
use crate::verror::{OrError, VError};

pub enum IValue {
    IInt,
    IString,
}

pub struct Interpreter {
    //for sdl
    sdl_context: sdl2::Sdl,
    canvas: sdl2::render::Canvas::<sdl2::video::Window>,
    epump: sdl2::EventPump,
    memory: Memory,
    ip: u64, // instruction pointer
    dom: DOMTree,
    global_var: Vec<(String, IValue)>, // hash or AVT tree should be used.
}

pub struct Config {
    title: String,
    window_w: u32,
    window_h: u32,
}

impl Interpreter {
    pub fn default() -> OrError<Interpreter> {
        Interpreter::new(&Config{
            title: "Dummy".to_string(), window_w: 800, window_h: 600})}
    pub fn new(cfg: &Config) -> OrError<Interpreter> {
        let sdl_context = sdl2::init().unwrap();
        let video_subsystem = sdl_context.video().unwrap();
        let window = video_subsystem.window(&cfg.title, cfg.window_w, cfg.window_h)
            .position_centered()
            .build()
            .unwrap();
        let mut canvas = window.into_canvas().build().unwrap();
        let mut event_pump = sdl_context.event_pump().unwrap();
        Ok(Interpreter {
            sdl_context: sdl_context,
            canvas: canvas,
            epump: event_pump,
            memory: Memory::new(),
            ip: 0,
            dom: DOMTree::root(),
            global_var: Vec::new(),})
    }

    pub fn update_screen(&mut self) -> () {
        self.canvas.set_draw_color(Color::RGB(0, 255, 255));
        self.canvas.clear();
        self.canvas.present();
    } 

    pub fn run(self) -> OrError<()> {
        let mut interp = self;
        'running: loop {
            let instr = interp.memory.fetch(interp.ip)?;
            match instr {
                UpdateGVar => {
                    interp.update_screen();
                },
                Quit => break 'running,
                _ => { Err(VError::Unimplemented("undefinied inistr"))? },
            }
        };
        Err(VError::Unimplemented("interpreter.run()"))
    }
}

mod tests {
    use super::*;
    #[test]
    fn test1() -> OrError<()> {
        let intp = Interpreter::new();
        intp.run()
    }
}

//use std::cell::RefCell;
/*
use sdl2::event::Event;
use sdl2::pixels::Color;
use sdl2::keyboard::Keycode;

use crate::verror::{OrError, VError};
use crate::project::{Project, Scene, Layout};

use create::instruction::{Instruction}; */

/*
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
    pub insts: Vec<Instruction>,
    pub cur_inst: &'a Instruction,
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
                        insts:,
                        cur_inst:,
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
}*/
