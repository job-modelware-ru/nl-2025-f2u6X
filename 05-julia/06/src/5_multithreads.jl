# using Base.Threads — подключает модуль многопоточности
# Многопоточность: потоки разделяют общую память
using Base.Threads

# nthreads(), threadid() — возвращают количество доступных потоков и идентификатор текущего потока
println("Доступно потоков: ", nthreads())
println("Текущий поток: ", threadid())

function parallel_array_operation(arr)
    # similar(arr) — создает массив того же размера и типа
    result = similar(arr)
    # @threads — распределяет итерации цикла по потокам
    # Автоматически делит диапазон итераций между потоками
    Threads.@threads for i in eachindex(arr)
        # Каждый поток работает со своей частью массива (разделяемая память)
        result[i] = arr[i] * arr[i] + sin(arr[i])
    end
    return result
end

data = rand(1000) # Создаём массив случайных чисел
@time parallel_array_operation(data) # Измеряем время выполнения

# Корректная параллельная редукция с локальными переменными для каждого потока
function parallel_sum(arr)
    # Локальная сумма для каждого потока, размерность = кол-во потоков
    local_sums = zeros(Float64, nthreads())
    Threads.@threads for i in eachindex(arr)
        tid = threadid()
        # Избегаем bounds ошибки: если потоков больше, чем выделено в local_sums, это приведет к ошибке;
        # но local_sums создаётся специального размера, так что защищено.
        @inbounds local_sums[tid] += arr[i]
    end
    return sum(local_sums)
end

large_data = rand(10^6)
sequential_time = @elapsed sum(large_data)
parallel_time = @elapsed parallel_sum(large_data)
println("Ускорение: $(sequential_time / parallel_time)x")