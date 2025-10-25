class Person
  attr_accessor :name, :age
end

p = Person.new
p.name = "Alice"
p.age = 25
puts "#{p.name} is #{p.age} years old"
