use lazy_static::lazy_static;
lazy_static! {
    static ref NUMARR: Vec<i32> = { (1..=100).map(|num| num * num).collect() };
}

fn main() {
    print!("start");
    println!("{:?}", *NUMARR);
}
