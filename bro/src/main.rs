use pigeon::crow;

fn add(a: i32, b: i32) -> i32 {
    a + b
}

fn mul(a: i32, b: i32) -> i32 {
    a * b
}

fn always_true() -> bool {
    true
}

fn always_one() -> i32 {
    1
}

fn always_zero() -> i32 {
    0
}

fn main() {
    let _x = crow!((add 1 2));
    let _y = crow!((let [a 2 b 3] (add a b)));
    let _z = crow!((let [a 11 b 4 c 3] (mul a (add b c))));

    // let add_and_mul = crow!((fn [a b] [i32 i32 i32] (mul a (add b 1))) );
    // let res = crow!((add_and_mul 7 10));
    // assert_eq!(77, res);

    // let blah = crow! {(fn [a b] [&&&&std::collections::VecDeque<i32> i32 i32] (add 1 2))};
    // let dq = std::collections::VecDeque::new();
    // let x = crow!((blah &&&& dq 5i32));
    // assert_eq!(x, 3);

    let five_oh = crow!((if (always_true) (always_one) (always_zero)));

    let _oh_no = crow!((if (always_true)  (add (mul 3 2) 1) 0));
    assert_eq!(five_oh, 1);
    let _x = crow!((let [a 11 b 4 c 3] (mul a (add b c))));
    // assert_eq!(77, x)
    assert_eq!(
        {
            {
                5
            }
        },
        5
    )
}
