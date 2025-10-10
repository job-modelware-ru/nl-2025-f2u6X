use std::sync::{Arc, Mutex};
use std::thread;

pub fn run_example() {
    println!("\n--- Пример: Совместное состояние (Arc<Mutex>) ---");

    let counter = Arc::new(Mutex::new(0));
    let mut handles = vec![];

    for _ in 0..5 {
        let counter_clone = Arc::clone(&counter);
        let handle = thread::spawn(move || {
            let mut num = counter_clone.lock().unwrap();
            *num += 1;
        });
        handles.push(handle);
    }

    for h in handles {
        h.join().unwrap();
    }

    println!("Результат счётчика: {}\n", *counter.lock().unwrap());
}
