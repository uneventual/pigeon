use crow::crow;

fn add(a: i32, b: i32) -> i32 {
    a + b
}

fn main() {
    let x = crow!((add 1 2));
    println!("Hello, world! {}", x);
}
