using Distributed

# @spawnat - выполняет код на указанном процессе
# 2 - номер целевого процесса (один из рабочих процессов)
# Возвращает RemoteRef - ссылку на удалённый результат
if nprocs() < 2
    addprocs(2 - nprocs() + 1)
end

remote_ref = @spawnat 2 begin
    # Этот код выполняется ТОЛЬКО на процессе 2
    sum = 0
    for i in 1:1000
        sum += i # Суммируем числа от 1 до 1000
    end
    sum # Результат вычислений на процессе 2
end

# fetch - получает результат удалённого вычисления
# Блокирующая операция - ждёт завершения вычисления на удалённом процессе
result = fetch(remote_ref)
println("Сумма от 1 до 1000 (вычислено на процессе 2): ", result)

# pmap - параллельное отображение функции на коллекцию
# Распределяет вызовы функции по доступным процессам
data = [1, 2, 3, 4, 5]
# x -> x * x - лямбда-функция, вычисляющая квадрат
squares = pmap(x -> x * x, data) # [1, 4, 9, 16, 25]
println("Квадраты чисел: ", squares)

# remotecall - альтернативный способ удалённого вызова
# remotecall(функция, процесс, аргументы...)
remote_square = remotecall(x -> x * x, 2, 10)
square_result = fetch(remote_square)
println("Квадрат 10 (вычислено на процессе 2): ", square_result)

# @everywhere для сложных вычислений
@everywhere function matrix_multiply(A, B)
    return A * B # Умножение матриц на каждом процессе
end