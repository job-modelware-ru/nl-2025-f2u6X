use std::fs::File;

// ================================
// IF – if trả về giá trị
// ================================
fn example_if() {
    let x = 5;

    if x > 0 {
        println!("Положительное");
    } else {
        println!("Неположительное");
    }

    let number = if x % 2 == 0 { "чётное" } else { "нечётное" };
    println!("Число {} — {}", x, number);
}

// ================================
// Циклы: loop, while, for
// ================================
fn example_loops() {
    // loop + break
    loop {
        println!("Вечный цикл (один раз)");
        break;
    }

    // while
    let mut i = 0;
    while i < 5 {
        println!("i = {}", i);
        i += 1;
    }

    // for
    for x in 0..5 {
        println!("for x = {}", x);
    }
}

// ================================
// loop + break с возвращаемым значением
// ================================
fn example_loop_value() {
    let (mut a, mut b) = (100, 52);

    let result = loop {
        if a == b {
            break a;
        }
        if a > b {
            a -= b;
        } else {
            b -= a;
        }
    };

    println!("Result = {}", result);
}

// ================================
// Метки (labels)
// ================================
fn example_labels() {
    'outer: for x in 1..5 {
        println!("x = {}", x);
        let mut i = 0;

        while i < x {
            println!("  i = {}", i);
            i += 1;

            if i == 3 {
                break 'outer;
            }
        }
    }
}

// ================================
// Итератор
// ================================
struct Counter {
    count: u32,
}

impl Iterator for Counter {
    type Item = u32;

    fn next(&mut self) -> Option<Self::Item> {
        if self.count < 10 {
            self.count += 1;
            Some(self.count)
        } else {
            None
        }
    }
}

fn example_iterator() {
    let mut counter = Counter { count: 0 };
    for number in &mut counter {
        println!("Counter: {}", number);
    }
}

// ================================
// IntoIterator
// ================================
struct MyCollection {
    data: Vec<i32>,
}

impl MyCollection {
    fn new() -> Self {
        Self { data: Vec::new() }
    }

    fn add(&mut self, value: i32) {
        self.data.push(value);
    }
}

impl IntoIterator for MyCollection {
    type Item = i32;
    type IntoIter = std::vec::IntoIter<i32>;

    fn into_iter(self) -> Self::IntoIter {
        self.data.into_iter()
    }
}

fn example_into_iterator() {
    let mut c = MyCollection::new();
    c.add(2);
    c.add(4);
    c.add(8);

    for item in c {
        println!("Item: {}", item);
    }
}

// ================================
// match
// ================================
fn example_match() {
    let number = 3;

    match number {
        1 => println!("Один"),
        2 | 3 => println!("Два или три"),
        4..=10 => println!("От 4 до 10"),
        _ => println!("Другое"),
    }
}

// ================================
// Option<T>
// ================================
fn divide(a: i32, b: i32) -> Option<i32> {
    if b == 0 {
        None
    } else {
        Some(a / b)
    }
}

fn example_option() {
    match divide(10, 2) {
        Some(result) => println!("Результат: {}", result),
        None => println!("Деление на ноль!"),
    }
}

// ================================
// Result<T, E>
// ================================
fn example_result() {
    let file = File::open("test.txt");

    match file {
        Ok(_) => println!("Файл открыт"),
        Err(e) => println!("Ошибка: {}", e),
    }
}

// ================================
// panic!
// ================================
fn example_panic() {
    assert!(2 + 2 == 4);
    // assert!(2 + 2 == 5); // ← раскомментируй для panic
    // panic!("Что-то пошло не так!");
}

// ================================
// main
// ================================
fn main() {
    println!("--- IF ---");
    example_if();

    println!("\n--- LOOPS ---");
    example_loops();

    println!("\n--- LOOP VALUE ---");
    example_loop_value();

    println!("\n--- LABELS ---");
    example_labels();

    println!("\n--- ITERATOR ---");
    example_iterator();

    println!("\n--- INTO ITERATOR ---");
    example_into_iterator();

    println!("\n--- MATCH ---");
    example_match();

    println!("\n--- OPTION ---");
    example_option();

    println!("\n--- RESULT ---");
    example_result();

    println!("\n--- PANIC ---");
    example_panic();
}
