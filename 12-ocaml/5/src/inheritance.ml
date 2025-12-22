class colored_point x y color =
  object
    inherit point x y as super
    
    val mutable color = color

    method get_color = color
    method set_color new_color = color <- new_color

    method print =
      Printf.printf "ColoredPoint(%d, %d, %s)\n"
        (super#get_x) (super#get_y) color
end