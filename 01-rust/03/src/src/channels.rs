use std::sync::mpsc;
use std::thread;
use std::time::Duration;

pub fn run_example() {
    println!("\n--- Пример: Каналы ---");

    let (tx, rx) = mpsc::channel();

    thread::spawn(move || {
        let messages = vec!["Привет", "из", "потока"];
        for msg in messages {
            tx.send(msg).unwrap();
            thread::sleep(Duration::from_millis(400));
        }
    });

    for received in rx {
        println!("Главный поток получил: {}", received);
    }

    println!("Канал завершён.\n");
}
