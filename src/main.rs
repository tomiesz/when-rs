use crate::c::c;
use crate::d::d;
use clap::Parser;

pub mod c;
pub mod d;
pub mod i;
pub mod lib;
pub mod parser;

fn main() {
    let cli = Args::parse();
    match cli.command {
        'i' => print!("default behaviour"), //default behaviour,
        'c' => c(),                         //print calendar,
        'e' => todo!(),                     //open file in your editor
        'w' => todo!(),
        'm' => todo!(),
        'y' => todo!(),
        'j' => todo!(), //print the modified julian day
        'd' => d(),     //print current date
        _ => panic!(),  //throw some error
    };
}

#[derive(Parser, Debug)]
#[clap(long_about=None)]
pub struct Args {
    #[clap(default_value = "i")]
    pub command: char,
}
