class Animal
  def speak
    puts "Some animal makes a sound"
  end
end

class Dog < Animal
  def speak
    puts "Woof!"
  end
end

class Cat < Animal
  def speak
    puts "Meow!"
  end
end

[Dog.new, Cat.new].each(&:speak)

