class Car
  def initialize(model)
    @model = model
  end

  def start
    puts "#{@model} started!"
  end
end

car = Car.new("Tesla")
car.start