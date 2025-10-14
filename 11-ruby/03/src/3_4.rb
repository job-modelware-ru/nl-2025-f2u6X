module Flyable
  def fly
    puts "#{self.class} is flying!"
  end
end

class Bird
  include Flyable
end

class Superhero
  include Flyable
end

Bird.new.fly       
Superhero.new.fly  
