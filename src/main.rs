extern crate clap;
extern crate xml;

mod verror;
mod project;
mod vm;
//mod compiler;

//use vconfig::{Config};
use clap::{App, Arg, SubCommand, AppSettings};
use project::Project;
use verror::{OrError, VError};
use vm::VM;

fn run (matches: &&clap::ArgMatches<'_>) -> OrError<()> {
/*    let project_file = matches.value_of("filename").unwrap();
    let project = Project::load_project(&project_file)?;
    let icfg = &project.config.interp_cfg;
    let mut interp = Interpreter::new(icfg)?;
//    let mut interp = Interpreter::default()?;// TODO: should be replaced with new(cfg)
    for c in project.components.iter() {
        let mut scr = c.load_script()?;
        interp.memory.load(&scr.name, &mut scr.body);
    }
    interp.run()?;
//    let interp = Interpreter::new(&project)?;
//    interp.run() */
    Err(VError::Unimplemented("Subcommand run is unimplemented"))
}

fn test_interp (_matches: &&clap::ArgMatches<'_>) -> OrError<()> {
    use vm::instr::Instruction;
    let mut test_code = vec![Instruction::Quit];
    let dummy_cfg = vm::Config::default();
    let mut vm = VM::new(&dummy_cfg)?;
    vm.memory.load("entry", &mut test_code);
    vm.run()?;
    Ok(())
}

fn main() {
    let app = App::new("VeNGE a Verified Novel Game Engine")
        .version("0.0.0")
        .author("Jack Alltrades <magikku009@hotmail.co.jp>")
        .about("")
        .setting(AppSettings::ArgRequiredElseHelp)
        .subcommand(
            SubCommand::with_name("run")
                .about("run a novel game scripts")
                .arg(Arg::with_name("filename")
                     .help("project file (.vproject)")
                     .required(true)
                )
        )
        .subcommand(
            SubCommand::with_name("build")
                .about("to make a relase build")
                .arg(Arg::with_name("filename")
                     .help("config file (.vngc)")
                     .required(true)
                )
        )
        .subcommand(
            SubCommand::with_name("ui")
                .about("to check a UI design")
                .arg(Arg::with_name("filename")
                     .help("layout file (.vngl)")
                     .required(true)
                )
                .arg(Arg::with_name("configfile")
                     .help("config file (.vngc)")
                     .short("c")
                     .long("config-file")
                     .takes_value(true)
                     .required(true)
                )
        )
        .subcommand(
            SubCommand::with_name("test-interp")
                .about("only for development")
                /*.arg(Arg::with_name("filename")
                     .required(true)
                )*/
        );
    let matches = app.get_matches();
    if let Some(ref matches) = matches.subcommand_matches("run") {
        match run(matches) {
            Ok(()) => (),
            Err(err) => println!("Error: {}", err)
        }
    }
    if let Some(ref _matches) = matches.subcommand_matches("build") {
        println!("Build");
    }
    if let Some(ref matches) = matches.subcommand_matches("ui") {
        println!("unimplemented");
    }
    if let Some(ref matches) = matches.subcommand_matches("test-interp") {
        match test_interp(matches) {
            Ok(()) => (),
            Err(err) => println!("Error: {}", err),
        }
    }
}
