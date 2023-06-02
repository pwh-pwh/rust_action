use std::num::ParseIntError;
use structopt::StructOpt;

#[derive(StructOpt, Debug)]
struct Opt {
    #[structopt(short = "l", long = "level")]
    info: bool,
    #[structopt(parse(try_from_str=num_parse))]
    num: usize,
}

fn num_parse(s: &str) -> Result<usize, String> {
    let num: usize = s.parse().map_err(|_| format!("can not parse"))?;
    if (1usize..=20usize).contains(&num) {
        Ok(num)
    } else {
        Err("num not in 1..10 err".into())
    }
}

fn main() {
    let opt = Opt::from_args();
    println!("{opt:?}");
}
