using Base.Threads

# Atomic{Int}(0) — создаёт атомарную целочисленную переменную
# Атомарные операции неделимы — гарантируют корректность при параллельном доступе
function thread_safe_counter(n_iterations)
    counter = Atomic{Int}(0) # Инициализируем атомарный счётчик нулём
    # Параллельный цикл с множеством потоков
    Threads.@threads for i in 1:n_iterations
        # atomic_add! — атомарно увеличивает счётчик на 1
        # Гарантирует, что даже при одновременном доступе не будет гонки данных
        atomic_add!(counter, 1)
    end
    return counter[] # counter[] — получаем значение атомарной переменной
end

# atomic_sub!(counter, value) — атомарное вычитание
# atomic_xchg!(counter, value) — атомарная замена значения

println("Безопасный счётчик: ", thread_safe_counter(10000))