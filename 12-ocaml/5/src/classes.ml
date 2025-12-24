class point x_init y_init =
  object
    val mutable x = x_init
    val mutable y = y_init
    
    method get_x = x
    method get_y = y
    method set_x new_x = x <- new_x
    method set_y new_y = y <- new_y
    method move dx dy =
      x <- x + dx;
      y <- y + dy
    method print =
      Printf.printf "Point(%d, %d)\n" x y
end

let my_own_point = new point 10 20;;
my_own_point#print;; (* Point(10, 20) *)
my_own_point#set_x 4;;
my_own_point#set_y 5;;
my_own_point#print;; (* Point(4, 5) *)