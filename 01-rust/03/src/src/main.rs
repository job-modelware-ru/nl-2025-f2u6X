mod threads;
mod channels;
mod sync;
mod send_sync;

fn main() {
    println!("=== Конкурентность и параллелизм в Rust ===");
    println!("Выберите пример для запуска:");
    println!("1. Потоки (Threads)");
    println!("2. Каналы (Channels)");
    println!("3. Совместное состояние (Arc<Mutex>)");
    println!("4. Трейты безопасности (Send/Sync)");

    // Đơn giản chạy tất cả ví dụ
    threads::run_example();
    channels::run_example();
    sync::run_example();
    send_sync::run_example();
}
