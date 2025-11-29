class Robot
  [:say_hello, :say_goodbye, :dance].each do |action|
    define_method(action) { puts "Robot is #{action}" }
  end
end

r = Robot.new
r.say_hello
r.dance
