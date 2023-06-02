use rand::{thread_rng, Rng};

fn main() {
    let mut rng = thread_rng();
    (0..10).for_each(|_| println!("{}", rng.gen_range(100i32..400i32)))
}
