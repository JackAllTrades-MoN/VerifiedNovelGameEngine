pub mod config;
pub mod dom;
pub mod instr;
pub mod memory;
pub mod script;
//pub mod drawable;
pub mod screen;

//use ini::Ini;
//use serde_derive::Deserialize;
use sdl2::event::Event;
use queues::*;

//use instr::Instruction;
use memory::Memory;
use crate::verror::{OrError, VError/*, MayNecessary*/};
use screen::{Screen};

pub type Config = config::Config;

pub struct VM<'a, 'b> {
    pub screen: Screen<'a, 'b>,
    pub memory: Memory,
    ip: u64, // instruction pointer
    dom: dom::DOMTree,
    event_queue: Queue<Event>,
}

impl<'a, 'b> VM<'a, 'b> {
    pub fn new(cfg: &Config) -> OrError<VM> {
        let screen = Screen::new(&cfg.screen)?;
        let memory = Memory::new();
        let ip = 0;
        let dom = dom::root();
        let event_queue = queue![];
        Ok(VM {screen, memory, ip, dom, event_queue})
    }

    pub fn run(self) -> OrError<()> {
        use instr::Instruction;
        let mut vm = self;
        'running: loop {
            let instr = vm.memory.fetch(vm.ip)?;
            println!("fetch: {:?}", &instr);
            for event in vm.screen.epump.poll_iter() {
                vm.event_queue.add(event);
            }
            match instr {
                Instruction::UpdateGVar => {
                    vm.screen.update();
                },
                Instruction::UpdateDOMAttr(id, attr_name, value) => {
                    let mut target = vm.dom.lookup_by_id(id)
                        .ok_or(VError::Other("DOM Not found".to_string()))?;
                    target.update_attr(attr_name, value.to_dom_value())?;
                    //vm.screen.update_drawable(&vm.dom);
                    vm.screen.update();
                },
                Instruction::Quit => { break 'running },
                _ => { Err(VError::Unimplemented("undefinied instruction"))? },
            }
            std::thread::sleep(std::time::Duration::from_millis(100));
            vm.ip = vm.ip + 1;
        };
        Ok(())
    }
}
