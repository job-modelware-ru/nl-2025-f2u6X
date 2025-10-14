class Robot
end

robot = Robot.new

# Add a method at runtime!
def robot.say_hi
  puts "Hello, I am a dynamic robot!"
end

robot.say_hi  
