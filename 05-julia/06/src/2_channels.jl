# Channel{Int}(5) — создаёт канал для целых чисел ёмкостью 5
# Канал — потокобезопасная очередь для обмена данными между задачами
ch = Channel{Int}(5)

# Производитель — задача, которая отправляет данные в канал
@sync begin
    for i = 1:3
        # put! — отправляет значение в канал (блокируется, если канал полон)
        put!(ch, i)
        println("Производитель отправил: ", i)
        sleep(0.2) # Имитация времени на производство данных
    end
end

# Потребитель — задача, которая получает данные из канала
@sync begin
    for i = 1:3
        # take! — получает значение из канала (блокируется, если канал пуст)
        value = take!(ch)
        println("Потребитель получил: ", value)
    end
end

# close(ch) — закрывает канал, сигнализируя о завершении работы
# isopen(ch) — проверяет, открыт ли канал

function producer_consumer_example()
    ch = Channel{String}(3)
    @sync begin
        names = ["Алиса", "Боб", "Чарли", "Дина"]
        for name in names
            if isopen(ch)
                put!(ch, name)
                println("Отправлено: $name")
            end
        end
        close(ch) # Закрываем канал после отправки всех данных
    end
    @sync begin
        while isopen(ch)
            # Безопасное получение данных с проверкой
            name = take!(ch)
            println("Получено: $name")
        end
    end
end