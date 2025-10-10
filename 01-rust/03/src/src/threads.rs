use std::thread;

pub fn run_example() {
    println!("\n--- Пример: Потоки ---");

    let handle = thread::spawn(|| {
        for i in 1..=3 {
            println!("Поток: {}", i);
        }
    });

    for i in 1..=3 {
        println!("Главный поток: {}", i);
    }

    handle.join().unwrap();
    println!("Поток завершён.\n");
}
