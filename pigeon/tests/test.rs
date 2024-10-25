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

#[test]
fn test_defn() {
    let add_and_mul = crow!((fn [a b] [i32 i32 i32] (mul a (add b 1))) );
    let res = crow!((add_and_mul 7 10));
    assert_eq!(77, res);
}

#[test]
fn more_defn() {
    let blah = crow! {(fn [a b] [&std::collections::VecDeque<i32> i32 i32] (add 1 2))};
    let col = std::collections::VecDeque::<i32>::new();
    let x = crow!((blah &col 5i32));
    assert_eq!(x, 3);
}
