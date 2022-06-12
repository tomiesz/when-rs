use crate::c::c;
use crate::d::d;
use crate::i::i;
use crate::j::j;
use chrono::prelude::*;
use clap::Parser;

pub mod c;
pub mod d;
pub mod i;
pub mod j;
pub mod lib;
pub mod parser;

fn main() {
    let cli = Args::parse();
    let eval_date = if let Some(date_str) = cli.now {
        let parts: Vec<&str> = date_str.trim().split_whitespace().collect();
        if parts.len() == 3 {
            NaiveDate::from_ymd(
                parts[0].parse().unwrap(),
                parts[1].parse().unwrap(),
                parts[2].parse().unwrap(),
            )
        } else {
            panic!("The date {} is not in the required format, which is 'y m d', with blanks separating the three parts.",date_str);
        }
    } else {
        chrono::Local::today().naive_local()
    };
    match cli.command {
        'i' => i(eval_date), //default behaviour,
        'c' => c(eval_date), //print calendar,
        'e' => todo!(),      //open file in your editor
        'w' => todo!(),
        'm' => todo!(),
        'y' => todo!(),
        'j' => j(eval_date), //print the modified julian day
        'd' => d(eval_date), //print current date
        _ => panic!(),       //throw some error
    };
}

#[derive(Parser, Debug)]
#[clap(long_about=None)]
pub struct Args {
    #[clap(default_value = "i")]
    pub command: char,
    #[clap(long)]
    pub now: Option<String>,
}
