using Base.Threads

# Реализация защищённого доступа с помощью блокировки (ReentrantLock)
function synchronized_operations()
    # ReentrantLock() — cоздаёт мьютекс (mutex) для синхронизации потоков
    lock = ReentrantLock()
    shared_data = [] # Общий ресурс для потоков

    # Параллельная запись в общий ресурс с защитой мьютексом
    @threads for i in 1:10
        # lock(lock) do ... end — захватывает мьютекс, автоматически освобождает по завершении, даже при ошибках
        lock(lock) do
            # Критическая секция: только один поток одновременно здесь
            push!(shared_data, (threadid(), i))
            println("Поток $(threadid()) добавил данные: $i")
        end
        # После выхода из блока lock(lock) мьютекс освобождается автоматически
    end
    return shared_data
end

# Альтернативный способ: явный захват/освобождение мьютекса с try/finally
function explicit_lock_example()
    lock = ReentrantLock()
    counter = 0
    @threads for i in 1:100
        lock(lock)
        try
            # Критическая секция
            counter += 1
            println("Поток $(threadid()) увеличил счётчик до: $counter")
        finally
            unlock(lock) # Гарантированное освобождение мьютекса даже при ошибках
        end
    end
    return counter
end

# try/finally гарантирует освобождение мьютекса даже при ошибках, что предотвращает deadlocks
