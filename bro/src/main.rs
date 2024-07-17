use crow::crow;

fn add(a: i32, b: i32) -> i32 {
    a + b
}


fn mul(a: i32, b: i32) -> i32 {
    a * b
}

fn main() {
    let x = crow!((add 1 2));
    let _y = crow!((let [a 2 b 3] (add a b)));
    let _z = crow!((let [a 11 b 4 c 3] (mul a (add b c))));
    println!("Hello, world! {}", x);
}
