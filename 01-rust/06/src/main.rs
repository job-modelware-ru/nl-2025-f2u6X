use std::fs::File;
use std::io::{self, Read, Write};

// =======================================
// 1. Срезы (Slices)
// =======================================
fn example_slices() {
    let arr = [1, 2, 3, 4, 5];
    let slice = &arr[1..4];
    println!("Array slice: {:?}", slice);

    let s = "Привет";
    let slice = &s[0..6]; // "При"
    println!("String slice: {}", slice);

    // ❌ panic (как в слайде)
    // let bad_slice = &s[0..5];
}

// =======================================
// 2. Box<T>
// =======================================
enum List {
    Cons(i32, Box<List>),
    Nil,
}

fn example_box() {
    let list = List::Cons(
        1,
        Box::new(List::Cons(2, Box::new(List::Nil))),
    );

    match list {
        List::Cons(value, _) => println!("List starts with {}", value),
        List::Nil => println!("Empty list"),
    }
}

// =======================================
// 3. Raw pointers + unsafe
// =======================================
fn example_raw_pointers() {
    let mut num = 10;

    let r1 = &num as *const i32;
    let r2 = &mut num as *mut i32;

    unsafe {
        println!("r1 = {}", *r1);
        *r2 = 20;
        println!("r2 = {}", *r2);
    }

    println!("num = {}", num);
}

// =======================================
// 4. I/O: println!, print!
// =======================================
fn example_io() {
    println!("Hello, Rust!");

    let name = "Alice";
    let age = 30;

    println!("Name: {}, Age: {}", name, age);
    println!("{1} is {0} years old", age, name);
    println!("{name} is {age} years old");

    let pi = 3.1415926535;
    println!("Pi is {:.2}", pi);
    println!("Binary: {:b}, Hex: {:x}", 42, 42);

    print!("Loading... ");
    println!("Done!");
}

// =======================================
// 5. stdin
// =======================================
fn example_stdin() {
    let mut input = String::new();

    println!("Введите ваше имя:");
    io::stdin()
        .read_line(&mut input)
        .expect("Не удалось прочитать строку");

    println!("Привет, {}!", input.trim());
}

// =======================================
// 6. write!
// =======================================
fn example_write_macro() -> io::Result<()> {
    let mut buffer = Vec::new();

    write!(&mut buffer, "Привет, мир!\n")?;
    write!(&mut buffer, "Второе сообщение\n")?;

    println!("Записано {} байт", buffer.len());

    let output = String::from_utf8(buffer).unwrap();
    print!("{}", output);

    Ok(())
}

// =======================================
// 7. Работа с файлами
// =======================================
fn write_string_to_file(path: &str, data: &str) -> io::Result<()> {
    let mut file = File::create(path)?;
    file.write_all(data.as_bytes())?;
    Ok(())
}

fn read_file_to_string(path: &str) -> io::Result<String> {
    let mut file = File::open(path)?;
    let mut contents = String::new();
    file.read_to_string(&mut contents)?;
    Ok(contents)
}

// =======================================
// 8. Копирование файла
// =======================================
fn copy_file(src: &str, dst: &str) -> io::Result<()> {
    let mut src_file = File::open(src)?;
    let mut dst_file = File::create(dst)?;

    let mut buffer = [0; 4096];

    loop {
        let bytes_read = src_file.read(&mut buffer)?;
        if bytes_read == 0 {
            break;
        }
        dst_file.write_all(&buffer[..bytes_read])?;
    }

    println!("Файл скопирован из {} в {}", src, dst);
    Ok(())
}

// =======================================
// main
// =======================================
fn main() {
    println!("--- SLICES ---");
    example_slices();

    println!("\n--- BOX ---");
    example_box();

    println!("\n--- RAW POINTERS & UNSAFE ---");
    example_raw_pointers();

    println!("\n--- IO ---");
    example_io();

    println!("\n--- STDIN ---");
    // example_stdin(); // ← включай при интерактивном запуске

    println!("\n--- WRITE! ---");
    example_write_macro().unwrap();

    println!("\n--- FILES ---");
    write_string_to_file("input.txt", "Hello from Rust!\n").unwrap();
    let content = read_file_to_string("input.txt").unwrap();
    println!("Read file:\n{}", content);

    println!("\n--- COPY FILE ---");
    copy_file("input.txt", "output.txt").unwrap();
}
