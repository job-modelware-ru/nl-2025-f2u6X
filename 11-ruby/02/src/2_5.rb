numbers = [1, 2, 3, 4, 5]
numbers.each { |n| puts n }
squares = numbers.map { |n| n**2 }
puts squares.inspect