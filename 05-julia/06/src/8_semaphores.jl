using Base.Threads
const Semaphore = Base.Semaphore
const acquire = Base.acquire
const release = Base.release

# Ограничение числа одновременно выполняющихся задач через семафор
function limited_concurrency()
    # Создаём семафор с максимальным числом параллельных доступов = 2
    sem = Semaphore(2)

    function limited_task(id)
        # Захватываем семафор (уменьшаем счетчик), если счетчик = 0 — задача ждёт.
        acquire(sem)
        try
            println("Задача $id начала выполнение")
            sleep(1) # Имитация работы
            println("Задача $id завершила выполнение")
        finally
            # Освобождаем семафор (увеличиваем счетчик)
            release(sem)
            println("Задача $id освободила семафор")
            printstyled("Задача $id освободила семафор\n", color=:red)
        end
    end

    # Запускаем 5 задач с ограничением одновременного выполнения
    @sync for i in 1:5
        @async limited_task(i)
        println("Задача $i запущена")
    end
end

# Демонстрация работы
limited_concurrency()