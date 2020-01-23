extern crate clap;
extern crate ini;
#[macro_use]
extern crate glium;
extern crate xml;
extern crate image;

//mod ui;
//mod vconfig;
//mod vngl;
mod verror;
mod project;
mod interpreter;

//use vconfig::{Config};
use clap::{App, Arg, SubCommand, AppSettings};
use project::Project;
use interpreter::{Interpreter};
use verror::{OrError, VError};

fn run (matches: &&clap::ArgMatches<'_>) -> OrError<()> {
    let project_file = matches.value_of("filename").unwrap();
    let project = Project::load_project(&project_file)?;
    let interp = Interpreter::default()?;// TODO: should be replaced with new(cfg)
    interp.run()?;
//    let interp = Interpreter::new(&project)?;
//    interp.run()
    Err(VError::Unimplemented("interpreter is not unimplemented"))
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
        );
    let matches = app.get_matches();
    if let Some(ref matches) = matches.subcommand_matches("run") {
        run(matches)
            .map_err(|err| println!("Error: {}", err));
    }
    if let Some(ref _matches) = matches.subcommand_matches("build") {
        println!("Build");
    }
    if let Some(ref matches) = matches.subcommand_matches("ui") {
        println!("unimplemented");
/*        let cfgfile = matches.value_of("configfile").unwrap();
        let lfile = matches.value_of("filename").unwrap();
        Config::parse_from_file(&cfgfile)
            .and_then(|cfg| ui::from_file(&cfg, &lfile))
            .map_err(|err| println!("Error: {}", err));*/
    }
}
