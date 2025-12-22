class virtual shape name =
  object
    method virtual area : float
    method virtual perimeter : float
    val name = name

    method get_name = name
    method print_info =
      Printf.printf "Shape: %s, Area: %.2f, Perimeter: %.2f\n"
        name (self#area) (self#perimeter)
end

class circle radius =
  object
    inherit shape "Circle"
    
    method area = 3.14159 *. radius *. radius
    method perimeter = 2. *. 3.14159 *. radius
end
