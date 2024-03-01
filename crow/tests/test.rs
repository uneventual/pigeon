use crow::crow;

fn add(a: i32, b: i32) -> i32 {
    a + b
}

fn mul(a: i32, b: i32) -> i32 {
    a * b
}

#[test]
fn test_add() {
    let x = crow!((mul (add 4 3) 11));
    assert_eq!(x, 77);
}
