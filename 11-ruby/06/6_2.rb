class CoffeeMachine
  def self.make(&block)
    machine = new
    machine.instance_eval(&block)
    machine.show_recipe
  end

  def initialize
    @ingredients = []
  end

  def add(item)
    @ingredients << item
  end

  def show_recipe
    puts "Coffee recipe:"
    @ingredients.each_with_index do |item, index|
      puts "#{index + 1}. #{item}"
    end
  end
end

# DSL to make coffee
CoffeeMachine.make do
  add 'Espresso'
  add 'Milk'
  add 'Sugar'
end

