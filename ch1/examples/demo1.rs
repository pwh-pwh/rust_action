use rand::{thread_rng, Rng};

fn main() {
    //生成100-400的10个随机数f32数字
    let mut rng = thread_rng();
    (0..10).for_each(|_| println!("{}", rng.gen_range(100.0f32..=400.0f32)))
}
