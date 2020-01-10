extern crate clap;
extern crate ini;
#[macro_use]
extern crate glium;
extern crate xml;
extern crate image;

mod ui;
mod vconfig;
mod verror;
mod vngl;

use clap::{App, Arg, SubCommand, AppSettings};
use vconfig::{Config};

fn main() {
    let app = App::new("VeNGE a Verified Novel Game Engine")
        .version("0.0.0")
        .author("Jack Alltrades <kashiwagi513@gmail.com>")
        .about("")
        .setting(AppSettings::ArgRequiredElseHelp)
        .subcommand(
            SubCommand::with_name("run")
                .about("run a novel game scripts")
                .arg(Arg::with_name("filename")
                     .help("config file (.vngc)")
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
    if let Some(ref _matches) = matches.subcommand_matches("run") {
        println!("Run");
    }
    if let Some(ref _matches) = matches.subcommand_matches("build") {
        println!("Build");
    }
    if let Some(ref matches) = matches.subcommand_matches("ui") {
        println!("UI");
        let cfgfile = matches.value_of("configfile").unwrap();
        let lfile = matches.value_of("filename").unwrap();
        Config::parse_from_file(&cfgfile)
            .and_then(|cfg| ui::from_file(&cfg, &lfile))
            .map_err(|err| println!("Error: {}", err));
    }
}
