use pigeon::crow;

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

#[test]
fn test_let() {
    let x = crow!((let [a 11 b 4 c 3] (mul a (add b c))));
    assert_eq!(77, x)
}
