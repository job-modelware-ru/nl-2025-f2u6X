use std::rc::Rc;
use std::sync::Arc;
use std::thread;

pub fn run_example() {
    println!("\n--- Пример: Send и Sync ---");

    // Rc<T> — не Send
    // Arc<T> — Send + Sync

    // Rc<T> нельзя передавать между потоками
    // let rc = Rc::new(5);
    // thread::spawn(move || println!("{}", rc)); // Ошибка компиляции

    let arc = Arc::new(5);
    let arc_clone = Arc::clone(&arc);

    let handle = thread::spawn(move || {
        println!("В другом потоке: {}", arc_clone);
    });

    handle.join().unwrap();
    println!("В главном потоке: {}", arc);
    println!("Send/Sync проверено.\n");
}
