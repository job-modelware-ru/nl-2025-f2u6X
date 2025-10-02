fn main() {
    println!("=== Rust Basics Demo ===");

    primitive_types();
    ownership_demo();
    borrowing_demo();
    recursion_demo();
    closure_demo();
    higher_order_demo();
}

fn primitive_types() {
    let x: i32 = 42;
    let y: f64 = 3.14;
    let b: bool = true;
    let c: char = 'R';

    println!("Primitives: x={}, y={}, b={}, c={}", x, y, b, c);
}

fn ownership_demo() {
    let s = String::from("hello");
    takes_ownership(s); // s moved
    let x = 5;
    makes_copy(x);
    println!("Ownership demo: x still usable = {}", x);
}

fn takes_ownership(some_string: String) {
    println!("Got ownership of: {}", some_string);
}

fn makes_copy(some_integer: i32) {
    println!("Got a copy: {}", some_integer);
}

fn borrowing_demo() {
    let mut s = String::from("hello");
    change(&mut s);
    println!("After borrowing change: {}", s);
}

fn change(s: &mut String) {
    s.push_str(", world");
}

fn recursion_demo() {
    fn factorial(n: u64) -> u64 {
        if n == 0 { 1 } else { n * factorial(n - 1) }
    }
    println!("Factorial(5) = {}", factorial(5));
}

fn closure_demo() {
    let add_one = |x: i32| x + 1;
    println!("Closure: 5+1 = {}", add_one(5));
}

fn higher_order_demo() {
    fn apply<F>(f: F, x: i32) -> i32 where F: Fn(i32) -> i32 {
        f(x)
    }
    let double = |x| x * 2;
    println!("Higher-order: apply double to 4 = {}", apply(double, 4));
}
