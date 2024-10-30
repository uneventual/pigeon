use pigeon::pigeon;

fn add(a: i32, b: i32) -> i32 {
    a + b
}

fn mul(a: i32, b: i32) -> i32 {
    a * b
}

#[test]
fn test_add() {
    let x = pigeon!((mul (add 4 3) 11));
    assert_eq!(x, 77);
}

#[test]
fn test_let() {
    let x = pigeon!((let [a 11 b 4 c 3] (mul a (add b c))));
    assert_eq!(77, x);
}

#[test]
fn test_defn() {
    let add_and_mul = pigeon!((fn [a b] [i32 i32 i32] (mul a (add b 1))) );
    let res = pigeon!((add_and_mul 7 10));
    assert_eq!(77, res);
}

#[test]
fn more_defn() {
    let blah = pigeon! {(fn [a b] [&&&&&std::collections::VecDeque<i32> i32 i32] (add 1 2))};
    let col = std::collections::VecDeque::<i32>::new();
    let x = pigeon!((blah &&&&&col 5i32));
    assert_eq!(x, 3);
}

#[test]
fn qualified_calls() {
    let x = pigeon!((std::collections::VecDeque::<i32>::new));
    assert_eq!(x.len(), 0);
}

fn always_true() -> bool {
    true
}

#[test]
fn test_calling_inline_function() {
    let res = pigeon!(( (fn [a b] [i32 i32 i32] (mul a (add b 1))) 7 10 ));
    assert_eq!(77, res);
}

fn leq(a: i32, b: i32) -> bool {
    a <= b
}

#[test]
fn test_loops() {
    let res = pigeon!((loop [a 0] (if (leq a 10000)  (recur [a (add a 1)]) a)));
    assert_eq!(res, 10001);
}

#[test]
fn test_negative_numbers() {
    let res = pigeon!((add 3 -1));
    assert_eq!(res, 2);
}

#[test]
fn test_if() {
    let five_oh = pigeon!((if (always_true) 5 0));
    assert_eq!(five_oh, 5);
    let oh_no = pigeon!((if (always_true)  (add (mul 3 2) 1) 0));
    assert_eq!(oh_no, 7);
}
